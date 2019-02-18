import * as React from 'react';
import { render } from 'react-dom';
import { List, Map } from 'immutable';

import { Mapping, Expansion, Message, Workspace, Either, Result, Pointer } from './types';
import { messageParser } from './parser';
import { postView, postReply, postWait, postNext, getJoin, getPointer } from './command-api';

/* PEG.js parser input
Top "message"
  = msgs:Msg+ { return {tag: 'Structured', contents: msgs}; }

Msg "submessage"
  = Pointer
  / [^\]\[$]+ { return {tag: 'Text', contents: text()}; }
  / "[" msgs:Msg+ "]" { return {tag: 'Structured', contents: msgs}; }

Pointer "pointer"
  = "$" digits:[0-9]+ { return {tag: 'Reference', contents: parseInt(digits.join(''), 10)}; }
*/

// TODO: Switch this to use a Map via withMutations.
function mappingFromMessage(mapping: {nextPointer: number, [ptr: number]: Pointer}, expansion: Expansion, msg: Message): void {
    switch(msg.tag) {
        case 'Text':
            return; // Nothing to do.
        case 'Reference':
            const p = msg.contents;
            if(!(p in mapping)) {
                mapping[p] = mapping.nextPointer++;
            }
            if(expansion.has(p)) {
                mappingFromMessage(mapping, expansion, expansion.get(p) as Message);
            }
            return;
        case 'Structured':
            msg.contents.forEach((m: Message) => mappingFromMessage(mapping, expansion, m));
            return;
        case 'LabeledStructured':
            const label = msg.contents[0];
            if(!(label in mapping)) {
                mapping[label] = mapping.nextPointer++;
            }
            msg.contents[1].forEach((m: Message) => mappingFromMessage(mapping, expansion, m));
            return;
        default:
            throw "Something's wrong";
    }
}

function mappingFromWorkspace(mapping: {nextPointer: number, [ptr: number]: Pointer}, workspace: Workspace): void {
    const expansion = workspace.expandedPointers;
    mappingFromMessage(mapping, expansion, workspace.question);
    workspace.subQuestions.forEach(q => {
        const answer = q[2];
        mappingFromMessage(mapping, expansion, q[1]);
        if(answer !== null) mappingFromMessage(mapping, expansion, answer); });
}

function renumberMessage(mapping: Mapping, msg: Message): Message {
    switch(msg.tag) {
        case 'Text':
            return msg;
        case 'Reference':
            return {tag: 'Reference', contents: mapping.get(msg.contents)};
        case 'Structured':
            return {tag: 'Structured', contents: msg.contents.map((m: Message) => renumberMessage(mapping, m))};
        default:
            throw "Something's wrong";
    }
}

const dummy: Element = document.createElement('textarea');
function escapeHTML(html: string): string {
    dummy.textContent = html;
    return dummy.innerHTML;
}

interface MessageProps {
    mapping: Mapping,
    expansion: Expansion,
    message: Message,
    isSubmessage?: boolean
}

const MessageComponent: React.FunctionComponent<MessageProps> = (props) => {
    const mapping = props.mapping;
    const expansion = props.expansion;
    const msg = props.message;
    switch(msg.tag) {
        case 'Text':
            return <span>{escapeHTML(msg.contents)}</span>;
        case 'Reference':
            const p = msg.contents;
            if(expansion.has(p)) {
                return <MessageComponent mapping={mapping} expansion={expansion} message={expansion.get(p) as Message} isSubmessage={true} />;
            } else {
                return <span className="pointer" data-original={p}>${mapping.get(p)}</span>;
            }
        case 'Structured':
            if(props.isSubmessage) {
                return <span>[{msg.contents.map((m: Message, i: number) =>
                                <MessageComponent key={i} mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}]
                       </span>;
            } else {
                return <span>{msg.contents.map((m: Message, i: number) =>
                                <MessageComponent key={i} mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}
                       </span>;
            }
        case 'LabeledStructured':
            const label: Pointer = msg.contents[0];
            return <span>[${mapping.get(label)}|{msg.contents[1].map((m: Message, i: number) =>
                            <MessageComponent key={i} mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}]
                   </span>;
    }
};

interface QuestionProps {
    mapping: Mapping,
    expansion: Expansion,
    question: Message
}

const QuestionComponent: React.FunctionComponent<QuestionProps> = (props) =>
    <div className="topLevelQuestion cell">
        <h2>
            <MessageComponent mapping={props.mapping} expansion={props.expansion} message={props.question} />
        </h2>
    </div>;

interface SubQuestionProps extends QuestionProps {
    answer: Message | null
}

const SubQuestionComponent: React.FunctionComponent<SubQuestionProps> = (props) => {
    const mapping = props.mapping;
    const expansion = props.expansion;
    const question = props.question;
    const answer = props.answer;
    if(answer === null) {
       return <div className="subQuestion unanswered">
                <div className="question"><MessageComponent mapping={mapping} expansion={expansion} message={question} /></div>
              </div>;
    } else {
       return <div className="subQuestion answered">
                <div className="question"><MessageComponent mapping={mapping} expansion={expansion} message={question} /></div>
                <div className="answer"><MessageComponent mapping={mapping} expansion={expansion} message={answer} /></div>
              </div>;
    }
};

class User {
    constructor(private readonly userId: number,
                readonly sessionId: number | null,
                private readonly pending = List<Either<Message, Pointer>>(),
                readonly workspace: Workspace | null = null,
                readonly mapping: Mapping = Map<Pointer, Pointer>(),
                private readonly inverseMapping: Mapping = Map<Pointer, Pointer>()) { }

    private get workspaceId(): number { return (this.workspace as Workspace).identity; }

    private static updateInverseMapping(transientMapping: {[ptr: number]: Pointer}): [Mapping, Mapping] {
        // There's probably a better way of doing this.
        const mapping: Array<[Pointer, Pointer]> = [];
        const invMapping: Array<[Pointer, Pointer]> = [];
        for(const k in transientMapping) {
            if(k === 'nextPointer') continue;
            const p1 = parseInt(k, 10);
            const p2 = transientMapping[k];
            mapping.push([p1, p2]);
            invMapping.push([p2, p1]);
        }
        return [Map<Pointer, Pointer>(mapping), Map<Pointer, Pointer>(invMapping)];
    }

    private postProcess(r: Result<void>): Result<User> {
        switch(r.tag) {
            case 'OK':
                return {tag: 'OK' as 'OK', contents: new User(this.userId, this.sessionId)};
            case 'Error':
            default:
                return {tag: 'Error' as 'Error'};
         }
    }

    ask(msg: Message): Promise<User> {
        const msg2 = renumberMessage(this.inverseMapping, msg);
        const ws = this.workspace as Workspace;
        const ws2: Workspace = {
            identity: ws.identity,
            expandedPointers: ws.expandedPointers,
            question: ws.question,
            subQuestions: ws.subQuestions.push([null, msg2, null])
        };
        const user = new User(this.userId,
                              this.sessionId,
                              this.pending.push({Left: msg2}),
                              ws2,
                              this.mapping,
                              this.inverseMapping);
        return new Promise((resolve, reject) => resolve(user));
    }

    reply(msg: Message): Promise<Result<User>> {
        return postReply([{userId:this.userId}, this.workspaceId, this.pending.toArray(), renumberMessage(this.inverseMapping, msg)])
               .then(r => this.postProcess(r.data));
    }

    view(ptr: Pointer): Promise<Result<User>> {
        return getPointer(ptr).then(r => {
            const msg: Message | null = r.data;
            if(msg !== null) {
                const ws = this.workspace as Workspace;
                const expansion = ws.expandedPointers;
                const ws2: Workspace = {
                    identity: ws.identity,
                    expandedPointers: expansion.set(ptr, msg),
                    question: ws.question,
                    subQuestions: ws.subQuestions
                };
                const transientMapping = this.mapping.toObject();
                transientMapping.nextPointer = this.mapping.size;
                mappingFromMessage(transientMapping as {[ptr: number]: Pointer, nextPointer: number}, expansion, msg);
                const mappings = User.updateInverseMapping(transientMapping);
                const user = new User(this.userId,
                                      this.sessionId,
                                      this.pending.push({Right: ptr}),
                                      ws2,
                                      mappings[0],
                                      mappings[1]);
                return {tag: 'OK' as 'OK', contents: user};
            }
            return {tag: 'Error' as 'Error'};
        });
    }

    wait(): Promise<Result<User>> {
        return postWait([{userId:this.userId}, this.workspaceId, this.pending.toArray()]).then(r => this.postProcess(r.data));
    }

    next(): Promise<User | null> {
        return postNext([{userId:this.userId}, this.sessionId]).then(response => {
            const workspaceSession = response.data;
            if(workspaceSession === null) return null;
            const ws = workspaceSession[0];
            const sessionId = workspaceSession[1];
            const expansion = ws.expandedPointers;
            const ep: Array<[Pointer, Message]> = [];
            for(const k in expansion) {
                const p = parseInt(k, 10);
                ep.push([p, expansion[p]]);
            }
            const ws2: Workspace = {
                identity: ws.identity,
                expandedPointers: Map<Pointer, Message>(ep),
                question: ws.question,
                subQuestions: List(ws.subQuestions)
            };
            const transientMapping = {nextPointer: 0};
            mappingFromWorkspace(transientMapping, ws2);
            const mappings = User.updateInverseMapping(transientMapping);

            const user = new User(this.userId,
                                  sessionId,
                                  List<Either<Message, Pointer>>(),
                                  ws2,
                                  mappings[0],
                                  mappings[1]);
            return user;
        });
    }
}

interface ButtonProps {
    label: string,
    onClick: (evt: React.MouseEvent) => void
}

const ButtonComponent: React.FunctionComponent<ButtonProps> = (props) =>
    <button type="submit" className="btn btn-outline-primary btn-default" onClick={props.onClick}>{props.label}</button>;

interface TextInputProps extends ButtonProps {
    className: string,
    inputText: string,
    onChange: (evt: React.ChangeEvent) => void
}

const TextInputComponent: React.FunctionComponent<TextInputProps> = (props) => {
    return <div className={props.className}>
            <input className="form-control" type="text" value={props.inputText} onChange={props.onChange}></input>
            <div className="input-group-append">
                <ButtonComponent onClick={props.onClick} label={props.label} />
            </div>
           </div>;
};

interface NewQuestionProps {
    inputText: string,
    onClick: (evt: React.MouseEvent) => void,
    onChange: (evt: React.ChangeEvent) => void
}

const NewQuestionComponent: React.FunctionComponent<NewQuestionProps> = (props) =>
    <form className="newQuestion cell">
        <TextInputComponent className="input-group" inputText={props.inputText} onChange={props.onChange} label="Ask" onClick={props.onClick} />
    </form>;

interface ReplyProps {
    inputText: string,
    onClick: (evt: React.MouseEvent) => void,
    onChange: (evt: React.ChangeEvent) => void
}

const ReplyComponent: React.FunctionComponent<ReplyProps> = (props) =>
    <form className="reply cell">
        <TextInputComponent className="input-group" inputText={props.inputText} onChange={props.onChange} label="Reply" onClick={props.onClick} />
    </form>;

interface WaitProps {
    onClick: (evt: React.MouseEvent) => void
}

const WaitComponent: React.FunctionComponent<WaitProps> = (props) =>
    <div className="wait cell"><ButtonComponent label="Wait" onClick={props.onClick} /></div>;

interface MainProps {
    userId: number,
    sessionId: number | null
}

interface MainState {
    user: User,
    askInputText: string,
    replyInputText: string
}

class MainComponent extends React.Component<MainProps, MainState> {
    state: MainState;

    constructor(props: MainProps) {
        super(props);
        this.state = {user: new User(props.userId, props.sessionId), askInputText: '', replyInputText: ''};
    }

    render() {
        const workspace = this.state.user.workspace;
        if(workspace === null) {
            return <div className="nextContainer"><ButtonComponent label="Next" onClick={this.nextClick} /></div>;
        } else {
            const sessionId = this.state.user.sessionId;
            location.hash = sessionId === null ? '' : '#' + sessionId;
            const mapping = this.state.user.mapping;
            const expansion = workspace.expandedPointers;
            console.log(workspace);
            return <div className="mainContainer" onClick={this.pointerClick}>
                       <QuestionComponent mapping={mapping} expansion={expansion} question={workspace.question} />
                       <div className="subQuestions cell">
                           {workspace.subQuestions.map((q, i) => // Using index-based keying is reasonable here.
                                <SubQuestionComponent key={i} mapping={mapping} expansion={expansion} question={q[1]} answer={q[2]} />)}
                       </div>
                       <NewQuestionComponent inputText={this.state.askInputText} onClick={this.askClick} onChange={this.askInputChange} />
                       <WaitComponent onClick={this.waitClick} />
                       <ReplyComponent inputText={this.state.replyInputText} onClick={this.replyClick} onChange={this.replyInputChange} />
                   </div>;
        }
    }

    askInputChange = (evt: React.ChangeEvent) => {
        const target = evt.target as HTMLInputElement;
        this.setState({user: this.state.user, askInputText: target.value, replyInputText: this.state.replyInputText});
    };

    replyInputChange = (evt: React.ChangeEvent) => {
        const target = evt.target as HTMLInputElement;
        this.setState({user: this.state.user, askInputText: this.state.askInputText, replyInputText: target.value});
    };

    pointerClick = (evt: React.MouseEvent) => {
        const target = evt.target as HTMLElement | null;
        if(target !== null && target.classList.contains('pointer')) {
            this.state.user.view(parseInt(target.dataset.original as string, 10)).then(r => {
                if(r.tag === 'OK') {
                    this.setState({user: r.contents, askInputText: this.state.askInputText, replyInputText: this.state.replyInputText});
                } else {
                    console.log(r);
                }
            });
            evt.preventDefault();
        } else {
            // Let it propagate.
        }
    };

    nextClick = (evt: React.MouseEvent) => {
        return this.state.user.next().then(user => {
            if(user === null) {
                // Do nothing but probably want to tell the user that.
            } else {
                this.setState({user: user, askInputText: '', replyInputText: ''});
            }
        });
    };

    askClick = (evt: React.MouseEvent) => {
        const msg = messageParser(this.state.askInputText);
        this.state.user.ask(msg).then(user => {
            this.setState({user: user, askInputText: '', replyInputText: this.state.replyInputText});
        });
    };

    replyClick = (evt: React.MouseEvent) => {
        const msg = messageParser(this.state.replyInputText);
        this.state.user.reply(msg).then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, askInputText: '', replyInputText: ''});
            } else {
                console.log(r);
            }
        });
    };

    waitClick = (evt: React.MouseEvent) => {
        this.state.user.wait().then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, askInputText: '', replyInputText: ''});
            } else {
                console.log(r);
            }
        });
    };
}

const mainDiv: HTMLElement = document.getElementById('main') as HTMLElement;
getJoin().then(joinResponse => {
    const userId = joinResponse.data.userId;
    const maybeSessionId = parseInt(location.hash.slice(1), 10);
    render(<MainComponent userId={userId} sessionId={isNaN(maybeSessionId) ? null : maybeSessionId} />, mainDiv);
}).catch(e => console.log(e));

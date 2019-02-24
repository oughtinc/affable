import * as React from 'react';
import { render } from 'react-dom';
import { List, Map } from 'immutable';
import matchSorter from 'match-sorter';
import Downshift, { StateChangeOptions, ControllerStateAndHelpers } from 'downshift';

import { Mapping, Expansion, Message, Workspace, Either, Result, Pointer } from './types';
import { messageParser } from './parser';
import { postView, postReply, postWait, postNext, getCompletions, getJoin, getPointer } from './command-api';

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

function messageShape(msg: Message, substitutes: Array<string>): string {
    switch(msg.tag) {
        case 'Text':
            return msg.contents;
        case 'Reference':
            return '[]';
        case 'Structured':
            let i = 0;
            return msg.contents.map((m: Message) => {
                if(m.tag === 'Text') return m.contents;
                if(substitutes.length > i++) return substitutes[i-1];
                return '[]';
            }).join('');
        default:
            throw "Shouldn't happen";
    }
}

function messageToString(msg: Message): string {
    switch(msg.tag) {
        case 'Text':
            return msg.contents;
        case 'Reference':
            return '$' + msg.contents;
        case 'Structured':
            return '[' + msg.contents.map(messageToString).join('') + ']';
        default:
            throw "Shouldn't happen";
    }
}

function getSubstitutes(msg: Message): Array<string> {
    switch(msg.tag) {
        case 'Text':
            return [];
        case 'Reference':
            return ['$' + msg.contents];
        case 'Structured':
            const substs: Array<string> = [];
            msg.contents.forEach((m: Message) => {
                switch(m.tag) {
                    case 'Text':
                        return;
                    default:
                        substs.push(messageToString(m));
                        return;
                }
            });
            return substs;
        default:
            throw "Shouldn't happen";
    }
}

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
                return <span className="pointer locked unexpanded" data-original={p}>${mapping.get(p)}</span>;
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
            return <span>
                    <span className="pointer expanded">{mapping.get(label)}</span>
                    <span className="pointer-bracket left">[</span>
                    {msg.contents[1].map((m: Message, i: number) =>
                            <MessageComponent key={i} mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}
                    <span className="pointer-bracket right">]</span>
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
    inputText: string,
    onChange: (evt: React.ChangeEvent) => void
}

const TextInputComponent: React.FunctionComponent<TextInputProps> = (props) => {
    return <div className="input-group">
            <input className="form-control" type="text" value={props.inputText} onChange={props.onChange}></input>
            <div className="input-group-append">
                <ButtonComponent onClick={props.onClick} label={props.label} />
            </div>
           </div>;
};

interface TypeAheadProps extends ButtonProps {
    selectedValue: string | null,
    completions: Array<Message>,
    onStateChange?: (options: StateChangeOptions<string>, stateAndHelpers: ControllerStateAndHelpers<string>) => void
}

const TypeAheadComponent: React.FunctionComponent<TypeAheadProps> = (props) => {
    return <Downshift selectedItem={props.selectedValue} onStateChange={props.onStateChange}>{downshift => {
            let matches: Array<string> = [];
            try {
                const completions = props.completions;
                const input = downshift.inputValue;
                if(input !== null) {
                    const msg = messageParser(input);
                    const substs = getSubstitutes(msg);
                    const shape = messageShape(msg, substs);
                    const items = completions.map(m => messageShape(m, substs));
                    matches = matchSorter(items, shape); // TODO: Limit the number of outputs by slicing matches.
                }
            } catch { // TODO: Probably make this tighter. It's to handle messageParser failing to parse which will be common.
                // Do nothing
            }
            return <div className="typeahead">
                    <div className="input-group">
                        <input {...downshift.getInputProps()} className="form-control" type="text"></input>
                        <div className="input-group-append">
                            <ButtonComponent onClick={props.onClick} label={props.label} />
                        </div>
                    </div>
                    <ul {...downshift.getMenuProps()} className={downshift.isOpen ? 'open' : 'closed'}>{
                        downshift.isOpen ? matches.map((item, i) =>
                            <li {...downshift.getItemProps({key: item, index: i, item: item})}
                                className={i == downshift.highlightedIndex ? 'highlighted' : ''}>
                                {item}
                            </li>
                        ) : null
                    }</ul>
                   </div>;
            }}
           </Downshift>;
};

interface NewQuestionProps {
    selectedValue: string | null,
    completions: Array<Message>,
    onStateChange?: (options: StateChangeOptions<string>, stateAndHelpers: ControllerStateAndHelpers<string>) => void,
    onClick: (evt: React.MouseEvent) => void
}

const NewQuestionComponent: React.FunctionComponent<NewQuestionProps> = (props) =>
    <form className="newQuestion cell">
        <TypeAheadComponent
            selectedValue={props.selectedValue}
            completions={props.completions}
            onStateChange={props.onStateChange}
            label="Ask"
            onClick={props.onClick} />
    </form>;

interface ReplyProps {
    inputText: string,
    onClick: (evt: React.MouseEvent) => void,
    onChange: (evt: React.ChangeEvent) => void
}

const ReplyComponent: React.FunctionComponent<ReplyProps> = (props) =>
    <form className="reply cell">
        <TextInputComponent inputText={props.inputText} onChange={props.onChange} label="Reply" onClick={props.onClick} />
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
    completions?: Array<Message>,
    askInputText: string | null,
    replyInputText: string
}

class MainComponent extends React.Component<MainProps, MainState> {
    state: MainState;

    constructor(props: MainProps) {
        super(props);
        this.state = {user: new User(props.userId, props.sessionId), askInputText: '', replyInputText: ''};
    }

    askStateChange = (changes: StateChangeOptions<string>) => {
        if(changes.hasOwnProperty('selectedItem')) {
            this.setState({...this.state, askInputText: changes.selectedItem as string | null});
        } else if(changes.hasOwnProperty('inputValue')) {
            this.setState({...this.state, askInputText: changes.inputValue as string | null});
        }
    }

    render() {
        const workspace = this.state.user.workspace;
        if(workspace === null) {
            return <div className="nextContainer"><ButtonComponent label="Next" onClick={this.nextClick} /></div>;
        } else {
            const sessionId = this.state.user.sessionId;
            location.hash = sessionId === null ? '' : '#' + sessionId;
            const mapping = this.state.user.mapping;
            const askInputText = this.state.askInputText;
            const completions = this.state.completions;
            const expansion = workspace.expandedPointers;
            return <div className="mainContainer" onClick={this.pointerClick}>
                       <QuestionComponent mapping={mapping} expansion={expansion} question={workspace.question} />
                       <div className="subQuestions cell">
                           {workspace.subQuestions.map((q, i) => // Using index-based keying is reasonable here.
                                <SubQuestionComponent key={i} mapping={mapping} expansion={expansion} question={q[1]} answer={q[2]} />)}
                       </div>
                       <NewQuestionComponent
                        selectedValue={askInputText}
                        completions={completions === void(0) ? [] : completions}
                        onStateChange={this.askStateChange}
                        onClick={this.askClick} />
                       <WaitComponent onClick={this.waitClick} />
                       <ReplyComponent inputText={this.state.replyInputText} onClick={this.replyClick} onChange={this.replyInputChange} />
                   </div>;
        }
    }

    replyInputChange = (evt: React.ChangeEvent) => {
        const target = evt.target as HTMLInputElement;
        this.setState({...this.state, replyInputText: target.value});
    };

    pointerClick = (evt: React.MouseEvent) => {
        const target = evt.target as HTMLElement | null;
        if(target !== null && target.classList.contains('pointer') && target.classList.contains('locked')) {
            this.state.user.view(parseInt(target.dataset.original as string, 10)).then(r => {
                if(r.tag === 'OK') {
                    this.setState({...this.state, user: r.contents});
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
                return getCompletions(user.sessionId as number).then(r => {
                    this.setState({user: user, completions: r.data, askInputText: '', replyInputText: ''});
                });
            }
        });
    };

    askClick = (evt: React.MouseEvent) => {
        const askInputText = this.state.askInputText;
        if(askInputText === null) return;
        const msg = messageParser(askInputText);
        this.state.user.ask(msg).then(user => {
            this.setState({...this.state, user: user, askInputText: ''});
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
getJoin(parseInt(localStorage.userId, 10)).then(joinResponse => {
    const userId = joinResponse.data.userId;
    localStorage.userId = userId;
    const maybeSessionId = parseInt(location.hash.slice(1), 10);
    render(<MainComponent userId={userId} sessionId={isNaN(maybeSessionId) ? null : maybeSessionId} />, mainDiv);
}).catch(e => console.log(e));

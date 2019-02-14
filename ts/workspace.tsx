import * as React from 'react';
import { render } from 'react-dom';
import { List, Map } from 'immutable';

import { Mapping, Expansion, Message, Workspace, Either, Result, Pointer } from './types';
import { messageParser } from './parser';
import { postView, postReply, postWait, postNext, getJoin, getPointer } from './command-api';

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
                return <span>[{msg.contents.map((m: Message) =>
                                <MessageComponent mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}]
                       </span>;
            } else {
                return <span>{msg.contents.map((m: Message) =>
                                <MessageComponent mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}
                       </span>;
            }
        case 'LabeledStructured':
            const label: Pointer = msg.contents[0];
            return <span>[${mapping.get(label)}|{msg.contents[1].map((m: Message) =>
                            <MessageComponent mapping={mapping} expansion={expansion} message={m} isSubmessage={true} />)}]
                   </span>;
    }
};

interface WorkspaceProps {
    mapping: Mapping,
    workspace: Workspace
}

const WorkspaceComponent: React.FunctionComponent<WorkspaceProps> = (props) => {
    const mapping = props.mapping;
    const workspace = props.workspace;
    const expansion = workspace.expandedPointers;
    console.log(workspace);

    // TODO: Improve this.
    return <div>
               Question: <MessageComponent mapping={mapping} expansion={expansion} message={workspace.question} />;
               <br/>
               {workspace.subQuestions.map((q, i) => {
                   const answer = q[2];
                   if(answer === null) {
                       return [<br/>, (i+1)+'. ', <MessageComponent mapping={mapping} expansion={expansion} message={q[1]} />];
                   } else {
                       return [<br/>, (i+1)+'. ', <MessageComponent mapping={mapping} expansion={expansion} message={q[1]} />,
                               <br/>, 'Answer: ', <MessageComponent mapping={mapping} expansion={expansion} message={answer} />];
                   }
               })}
           </div>;
};

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

class User {
    constructor(private readonly userId: number,
                private readonly sessionId: number,
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
            const ws = response.data;
            if(ws === null) return null;
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
                                  this.sessionId,
                                  List<Either<Message, Pointer>>(),
                                  ws2,
                                  mappings[0],
                                  mappings[1]);
            return user;
        });
    }
}

interface MainProps {
    userId: number,
    sessionId: number
}

interface MainState {
    user: User,
    inputText: string
}

class MainComponent extends React.Component<MainProps, MainState> {
    state: MainState;

    constructor(props: MainProps) {
        super(props);
        this.state = {user: new User(props.userId, props.sessionId), inputText: ''};
    }

    render() {
        const workspace = this.state.user.workspace;
        if(workspace === null) {
            return <button onClick={this.nextClick}>Next</button>;
        } else {
            return [<div onClick={this.pointerClick}>
                       <WorkspaceComponent mapping={this.state.user.mapping} workspace={workspace} />
                    </div>,
                    <input type="text" value={this.state.inputText} onChange={this.inputChange}></input>,
                    <button onClick={this.askClick}>Ask</button>,
                    <button onClick={this.replyClick}>Reply</button>,
                    <button onClick={this.waitClick}>Wait</button>];
        }
    }

    inputChange = (evt: React.ChangeEvent) => {
        const target = evt.target as HTMLInputElement;
        this.setState({user: this.state.user, inputText: target.value});
    };

    pointerClick = (evt: React.MouseEvent) => {
        const target = evt.target as HTMLElement | null;
        if(target !== null && target.classList.contains('pointer')) {
            this.state.user.view(parseInt(target.dataset.original as string, 10)).then(r => {
                if(r.tag === 'OK') {
                    this.setState({user: r.contents, inputText: this.state.inputText});
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
                this.setState({user: user, inputText: ''});
            }
        });
    };

    askClick = (evt: React.MouseEvent) => {
        const msg = messageParser(this.state.inputText);
        this.state.user.ask(msg).then(user => {
            this.setState({user: user, inputText: ''});
        });
    };

    replyClick = (evt: React.MouseEvent) => {
        const msg = messageParser(this.state.inputText);
        this.state.user.reply(msg).then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, inputText: ''});
            } else {
                console.log(r);
            }
        });
    };

    waitClick = (evt: React.MouseEvent) => {
        this.state.user.wait().then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, inputText: ''});
            } else {
                console.log(r);
            }
        });
    };
}

const mainDiv: HTMLElement = document.getElementById('main') as HTMLElement;
const maybeSessionId = parseInt(location.hash.slice(1), 10);
(isNaN(maybeSessionId) ? getJoin() : getJoin(maybeSessionId)).then(joinResponse => {
    const userId = joinResponse.data[0].userId;
    const sessionId = joinResponse.data[1];
    location.hash = '#' + sessionId;
    render(<MainComponent userId={userId} sessionId={sessionId} />, mainDiv);
}).catch(e => console.log(e));

import * as React from 'react';
import { render } from 'react-dom';
import { List, Map, Set } from 'immutable';
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
  / "[$" digits:[0-9]+ ": " msgs:Msg+ "]" { return {tag: 'LabeledStructured', contents: [parseInt(digits.join(''), 10), msgs]}; }
  / "[" msgs:Msg+ "]" { return {tag: 'Structured', contents: msgs}; }
  / [^\]\[$]+ { return {tag: 'Text', contents: text()}; }

Pointer "pointer"
  = "$" digits:[0-9]+ { return {tag: 'Reference', contents: parseInt(digits.join(''), 10)}; }
*/

const dummy: Element = document.createElement('textarea');
function escapeHTML(html: string): string {
    dummy.textContent = html;
    return dummy.innerHTML;
}

function messageShape(msg: Message, substitutes: Array<string> = []): string {
    let i = 0;
    switch(msg.tag) {
        case 'Text':
            return msg.contents;
        case 'Reference':
            return '[]';
        case 'Structured':
            return msg.contents.map(m => {
                if(m.tag === 'Text') return m.contents;
                if(substitutes.length > i++) return substitutes[i-1];
                return '[]';
            }).join('');
        case 'LabeledStructured': // TODO: Think about this.
            return msg.contents[1].map(m => {
                if(m.tag === 'Text') return m.contents;
                if(substitutes.length > i++) return substitutes[i-1];
                return '[]';
            }).join('');
        default:
            console.log(msg);
            throw "messageShape: Shouldn't happen";
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
        case 'LabeledStructured':
            return '[$' + msg.contents[0] + ': ' + msg.contents[1].map(messageToString).join('') + ']';
        default:
            console.log(msg);
            throw "messageToString: Shouldn't happen";
    }
}

function getSubstitutes(msg: Message): Array<string> {
    const substs: Array<string> = [];
    switch(msg.tag) {
        case 'Text':
            return substs;
        case 'Reference':
            return ['$' + msg.contents];
        case 'Structured':
            msg.contents.forEach(m => {
                switch(m.tag) {
                    case 'Text':
                        return;
                    default:
                        substs.push(messageToString(m));
                        return;
                }
            });
            return substs;
        case 'LabeledStructured': // TODO: Is this the right thing to do?
            msg.contents[1].forEach(m  => {
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
            console.log(msg);
            throw "getSubstitutes: Shouldn't happen";
    }
}

function addCompletion(completions: List<Message>, msg: Message): List<Message> {
    const shape = messageShape(msg);
    const shapes = completions.map(m => messageShape(m));
    if(shapes.findIndex(s => s === shape) !== -1) return completions;
    return completions.push(msg);
}

function mappingFromMessage(mapping: Mapping /* mutable */, expansion: Expansion, msg: Message, seen: Set<Pointer> = Set().asMutable()): void {
    switch(msg.tag) {
        case 'Text':
            return; // Nothing to do.
        case 'Reference':
            const p = msg.contents;
            if(seen.has(p)) return;
            seen.add(p);
            if(!(mapping.has(p))) {
                mapping.set(p, mapping.size);
            }
            if(expansion.has(p)) {
                mappingFromMessage(mapping, expansion, expansion.get(p) as Message, seen);
            }
            return;
        case 'Structured':
            msg.contents.forEach(m => mappingFromMessage(mapping, expansion, m), seen);
            return;
        case 'LabeledStructured':
            const label = msg.contents[0];
            if(seen.has(label)) return;
            seen.add(label);
            if(!(mapping.has(label))) {
                mapping.set(label, mapping.size);
            }
            msg.contents[1].forEach(m => mappingFromMessage(mapping, expansion, m), seen);
            return;
        default:
            console.log(msg);
            throw "mappingFromMessage: Something's wrong";
    }
}

function mappingFromWorkspace(mapping: Mapping /* mutable */, workspace: Workspace): void {
    const expansion = workspace.expandedPointers;
    mappingFromMessage(mapping, expansion, workspace.question);
    workspace.subQuestions.forEach(q => {
        const answer = q[2];
        mappingFromMessage(mapping, expansion, q[1]);
        if(answer !== null) mappingFromMessage(mapping, expansion, answer); });
}

function boundPointers(base: number, mapping: Mapping /* mutable */, msg: Message): void {
    switch(msg.tag) {
        case 'Structured':
            msg.contents.forEach(m => boundPointers(base, mapping, m));
            break;
        case 'LabeledStructured': // TODO: Check for reuse of labels.
            const p = base - mapping.size;
            mapping.set(msg.contents[0], p);
            msg.contents[1].forEach(m => boundPointers(base, mapping, m));
            break;
    }
}

function renumberMessage(invMapping: Mapping /* mutable */, msg: Message): [Message, Expansion] {
    const mn = invMapping.min();
    const base = mn === void(0) ? -1 : Math.min(-1, mn);
    const localMapping = Map<Pointer,Pointer>().withMutations(tm => boundPointers(base, tm, msg));

    localMapping.forEach(p => invMapping.set(invMapping.size, p));

    const expansion = Map<Pointer, Message>().asMutable();
    const loop: (m: Message) => Message = m => {
        switch(m.tag) {
            case 'Text':
                return m;
            case 'Reference':
                const ptr = localMapping.get(m.contents);
                if(ptr !== void(0)) {
                    return {tag: 'Reference', contents: ptr};
                } else {
                    return {tag: 'Reference', contents: invMapping.get(m.contents) as number};
                }
            case 'Structured':
                return {tag: 'Structured', contents: m.contents.map(loop)};
            case 'LabeledStructured':
                const p = localMapping.get(m.contents[0]) as number;
                const r: Message = {tag: 'LabeledStructured', contents: [p, m.contents[1].map(loop)]};
                expansion.set(p, r);
                return r;
            default:
                console.log(m);
                throw "renumberMessage: Something's wrong";
        }
    };
    return [loop(msg), expansion.asImmutable()];
}

interface MessageProps {
    mapping: Mapping,
    expansion: Expansion,
    expandedOccurrences: Set<string>,
    message: Message,
    path: string,
    isSubmessage?: boolean
}

const MessageComponent: React.FunctionComponent<MessageProps> = (props) => {
    const mapping = props.mapping;
    const expansion = props.expansion;
    const occurrences = props.expandedOccurrences;
    const msg = props.message;
    const path = props.path;
    switch(msg.tag) {
        case 'Text':
            return <span>{escapeHTML(msg.contents)}</span>;
        case 'Reference':
            const p = msg.contents;
            if(expansion.has(p)) {
                if(occurrences.has(path)) {
                    return <MessageComponent {...props} message={expansion.get(p) as Message} isSubmessage={true} />;
                } else {
                    return <span className="pointer unexpanded" data-path={path} data-original={p}>${mapping.get(p)}</span>;
                }
            } else {
                return <span className="pointer locked unexpanded" data-path={path} data-original={p}>${mapping.get(p)}</span>;
            }
        case 'Structured':
            if(props.isSubmessage) {
                return <span data-path={path}>[{msg.contents.map((m, i) =>
                                <MessageComponent {...props} key={i} message={m} path={path+'.'+i} isSubmessage={true} />)}]
                       </span>;
            } else {
                return <span data-path={path}>{msg.contents.map((m, i) =>
                                <MessageComponent {...props} key={i} message={m} path={path+'.'+i} isSubmessage={true} />)}
                       </span>;
            }
        case 'LabeledStructured':
            const label = msg.contents[0];
            return <span>
                    <span className={props.isSubmessage ? 'pointer expanded' : 'pointer expanded top'} data-path={path} data-original={label}>
                        {mapping.get(label)}
                    </span>
                    <span className="pointer-bracket left">[</span>
                    {msg.contents[1].map((m, i) =>
                            <MessageComponent {...props} key={i} message={m} path={path+'.'+i} isSubmessage={true} />)}
                    <span className="pointer-bracket right">]</span>
                   </span>;
    }
};

interface QuestionProps {
    mapping: Mapping,
    expansion: Expansion,
    expandedOccurrences: Set<string>,
    question: Message
}

const QuestionComponent: React.FunctionComponent<QuestionProps> = (props) =>
    <div className="topLevelQuestion cell">
        <h2>
            <MessageComponent mapping={props.mapping} expansion={props.expansion} expandedOccurrences={props.expandedOccurrences} message={props.question} path="top" />
        </h2>
    </div>;

interface SubQuestionProps extends QuestionProps {
    answer: Message | null,
    index: number
}

const SubQuestionComponent: React.FunctionComponent<SubQuestionProps> = (props) => {
    const msgProps = { mapping: props.mapping, expansion: props.expansion, expandedOccurrences: props.expandedOccurrences};
    const question = props.question;
    const answer = props.answer;
    const i = props.index;
    if(answer === null) {
       return <div className="subQuestion unanswered">
                <div className="question"><MessageComponent {...msgProps} message={question} path={'q.'+i}/></div>
              </div>;
    } else {
       return <div className="subQuestion answered">
                <div className="question"><MessageComponent {...msgProps} message={question} path={'q.'+i}/></div>
                <div className="answer"><MessageComponent {...msgProps} message={answer} path={'a.'+i}/></div>
              </div>;
    }
};

class User {
    constructor(private readonly userId: number,
                readonly sessionId: number | null,
                private readonly pending = List<Either<Message, Pointer>>(),
                readonly workspace: Workspace | null = null,
                readonly expandedOccurrences = Set<string>(),
                readonly mapping: Mapping = Map<Pointer, Pointer>(),
                private readonly inverseMapping: Mapping = Map<Pointer, Pointer>()) { }

    private get workspaceId(): number { return (this.workspace as Workspace).identity; }

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
        const ws = this.workspace;
        if(ws === null) throw 'User.ask: null workspace';
        let msg2: Message | null = null;
        let expansion: Expansion | null = null;
        const invMap = this.inverseMapping.withMutations(im => {
            const r = renumberMessage(im, msg);
            msg2 = r[0];
            expansion = r[1];
        });
        if(msg2 === null || expansion === null) throw "User.ask: something's wrong";
        const ws2: Workspace = {
            identity: ws.identity,
            expandedPointers: ws.expandedPointers.merge(expansion),
            question: ws.question,
            subQuestions: ws.subQuestions.push([null, msg2, null])
        };

        const user = new User(this.userId,
                              this.sessionId,
                              this.pending.push({Left: msg2}),
                              ws2,
                              this.expandedOccurrences,
                              invMap.mapEntries(entry => [entry[1], entry[0]]),
                              invMap)
        return new Promise((resolve, reject) => resolve(user));
    }

    reply(msg: Message): Promise<Result<User>> {
        let msg2: Message | null = null;
        this.inverseMapping.withMutations(im => { msg2 = renumberMessage(im, msg)[0]; })
        if(msg2 === null) throw "User.reply: something's wrong";
        return postReply([{userId:this.userId}, this.workspaceId, this.pending.toArray(), msg2])
               .then(r => this.postProcess(r.data));
    }

    view(ptr: Pointer, path: string): Promise<Result<User>> {
        const ws = this.workspace;
        if(ws === null) throw 'User.view: null workspace';
        const expansion = ws.expandedPointers;
        const occurrences = this.expandedOccurrences;

        if(expansion.has(ptr)) { // unlocked
            const user = new User(this.userId,
                                  this.sessionId,
                                  this.pending,
                                  ws,
                                  occurrences.has(path) ? occurrences.delete(path) : occurrences.add(path),
                                  this.mapping,
                                  this.inverseMapping);
            return new Promise((resolve, reject) => resolve({tag: 'OK' as 'OK', contents: user}));
        } else {
            return getPointer(ptr).then(r => {
                const msg: Message | null = r.data;
                if(msg !== null) {
                    const ws2: Workspace = {
                        identity: ws.identity,
                        expandedPointers: expansion.set(ptr, msg),
                        question: ws.question,
                        subQuestions: ws.subQuestions
                    };
                    const mapping = this.mapping.withMutations(tm => mappingFromMessage(tm, expansion, msg));
                    const invMapping = mapping.mapEntries(entry => [entry[1], entry[0]]);
                    const user = new User(this.userId,
                                          this.sessionId,
                                          this.pending.push({Right: ptr}),
                                          ws2,
                                          occurrences.add(path),
                                          mapping,
                                          invMapping);
                    return {tag: 'OK' as 'OK', contents: user};
                }
                return {tag: 'Error' as 'Error'};
            });
        }
    }

    wait(): Promise<Result<User>> {
        return postWait([{userId: this.userId}, this.workspaceId, this.pending.toArray()]).then(r => this.postProcess(r.data));
    }

    next(): Promise<User | null> {
        return postNext([{userId: this.userId}, this.sessionId]).then(response => {
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
            const mapping = Map<Pointer, Pointer>().withMutations(tm => mappingFromWorkspace(tm, ws2));
            const invMapping = mapping.mapEntries(entry => [entry[1], entry[0]]);
            const user = new User(this.userId,
                                  sessionId,
                                  List<Either<Message, Pointer>>(),
                                  ws2,
                                  Set<string>(),
                                  mapping,
                                  invMapping);
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
    completions: List<Message>,
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
                    matches = matchSorter(items.toArray(), shape); // TODO: Limit the number of outputs by slicing matches.
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
    completions: List<Message>,
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
    completions: List<Message>,
    askInputText: string | null,
    replyInputText: string
}

class MainComponent extends React.Component<MainProps, MainState> {
    state: MainState;

    constructor(props: MainProps) {
        super(props);
        this.state = {user: new User(props.userId, props.sessionId), completions: List<Message>(), askInputText: '', replyInputText: ''};
    }

    askStateChange = (changes: StateChangeOptions<string>) => {
        if(changes.hasOwnProperty('selectedItem')) {
            this.setState({askInputText: changes.selectedItem as string | null});
        } else if(changes.hasOwnProperty('inputValue')) {
            this.setState({askInputText: changes.inputValue as string | null});
        }
    }

    render() {
        const workspace = this.state.user.workspace;
        if(workspace === null) {
            return <div className="nextContainer"><ButtonComponent label="Next" onClick={this.nextClick} /></div>;
        } else {
            const sessionId = this.state.user.sessionId;
            location.hash = sessionId === null ? '' : '#' + sessionId;
            const ctxtProps = {
                mapping: this.state.user.mapping,
                expansion: workspace.expandedPointers,
                expandedOccurrences: this.state.user.expandedOccurrences};
            const askInputText = this.state.askInputText;
            const completions = this.state.completions;
            return <div className="mainContainer" onClick={this.pointerClick}>
                       <QuestionComponent {...ctxtProps} question={workspace.question} />
                       <div className="subQuestions cell">
                           {workspace.subQuestions.map((q, i) => // Using index-based keying is reasonable here.
                                <SubQuestionComponent key={i} index={i} {...ctxtProps} question={q[1]} answer={q[2]} />)}
                       </div>
                       <NewQuestionComponent
                        selectedValue={askInputText}
                        completions={completions}
                        onStateChange={this.askStateChange}
                        onClick={this.askClick} />
                       <WaitComponent onClick={this.waitClick} />
                       <ReplyComponent inputText={this.state.replyInputText} onClick={this.replyClick} onChange={this.replyInputChange} />
                   </div>;
        }
    }

    replyInputChange = (evt: React.ChangeEvent) => {
        const target = evt.target as HTMLInputElement;
        this.setState({replyInputText: target.value});
    };

    pointerClick = (evt: React.MouseEvent) => {
        const target = evt.target as HTMLElement | null;
        if(target !== null && (target.classList.contains('unexpanded') || target.classList.contains('expanded'))) {
            this.state.user.view(parseInt(target.dataset.original as string, 10), target.dataset.path as string).then(r => {
                if(r.tag === 'OK') {
                    this.setState({user: r.contents});
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
                    const q = (user.workspace as Workspace).question;
                    const completions = addCompletion(List<Message>(r.data), q);
                    this.setState({user: user, completions: completions, askInputText: '', replyInputText: ''});
                });
            }
        });
    };

    askClick = (evt: React.MouseEvent) => {
        const askInputText = this.state.askInputText;
        if(askInputText === null) return;
        const msg = messageParser(askInputText);
        this.state.user.ask(msg).then(user => {
            const q = (user.workspace as Workspace).subQuestions.last(null);
            if(q === null) throw "askClick: Shouldn't happen";
            this.setState(state => { return {user: user, completions: addCompletion(state.completions, q[1]), askInputText: ''}; });
        });
    };

    replyClick = (evt: React.MouseEvent) => {
        const msg = messageParser(this.state.replyInputText);
        this.state.user.reply(msg).then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, completions: List<Message>(), askInputText: '', replyInputText: ''});
            } else {
                console.log(r);
            }
        });
    };

    waitClick = (evt: React.MouseEvent) => {
        this.state.user.wait().then(r => {
            if(r.tag === 'OK') {
                this.setState({user: r.contents, completions: List<Message>(), askInputText: '', replyInputText: ''});
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

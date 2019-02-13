import { postView, postReply, postWait, postNext, getJoin, getPointer } from "./command-api";
import { messageParser } from "./parser";
import { Mapping, Expansion, Message, Workspace, Either, Result, Pointer } from "./types";

const workspaceDiv: HTMLElement = <HTMLElement>document.getElementById('workspace');
const inputTxt: HTMLInputElement = <HTMLInputElement>document.getElementById('inputTxt');
const nextBtn: HTMLButtonElement = <HTMLButtonElement>document.getElementById('nextBtn');
const workspaceContainerDiv: HTMLElement = <HTMLElement>document.getElementById('workspaceContainer');
const askBtn: HTMLButtonElement = <HTMLButtonElement>document.getElementById('askBtn');
const replyBtn: HTMLButtonElement = <HTMLButtonElement>document.getElementById('replyBtn');
const waitBtn: HTMLButtonElement = <HTMLButtonElement>document.getElementById('waitBtn');

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

const dummy: Element = document.createElement('textarea');
function escapeHTML(html: string): string {
    dummy.textContent = html;
    return dummy.innerHTML;
}

function renderMessage(mapping: Mapping, expansion: Expansion, msg: Message, isSubmessage = false): string {
    switch(msg.tag) {
        case 'Text':
            return escapeHTML(msg.contents);
        case 'Reference':
            const p = msg.contents;
            if(p in expansion) {
                return renderMessage(mapping, expansion, expansion[p], true);
            } else {
                return '<span class="pointer" data-original="'+p+'">$' + mapping[p] + '</span>';
            }
        case 'Structured': {
            const msgs = msg.contents.map((m: Message) => renderMessage(mapping, expansion, m, true));
            return isSubmessage ? '[' + msgs.join('') + ']' : msgs.join(''); }
        case 'LabeledStructured': {
            const label: Pointer = msg.contents[0];
            const msgs = msg.contents[1].map((m: Message) => renderMessage(mapping, expansion, m, true));
            return '[$' + mapping[label] + '|' + msgs.join('') + ']'; }
        default:
            throw "Something's wrong";
    }
}

function renderWorkspace(mapping: Mapping, workspace: Workspace): void {
    console.log(workspace);
    const expansion = workspace.expandedPointers;
    const questionText = 'Question: ' + renderMessage(mapping, expansion, workspace.question);
    const subquestionText = workspace.subQuestions.map((q, i) => {
        const answer = q[2];
        return '<br/>' + (i+1) + '. ' + renderMessage(mapping, expansion, q[1])
                       + (answer !== null ? '<br/>Answer: ' + renderMessage(mapping, expansion, answer) : '')
    }).join('<br/>');

    workspaceDiv.innerHTML = questionText + '<br/>' + subquestionText + '<br/>';
}

function mappingFromMessage(mapping: {nextPointer: number} & Mapping, expansion: Expansion, msg: Message): void {
    switch(msg.tag) {
        case 'Text':
            return; // Nothing to do.
        case 'Reference':
            const p = msg.contents;
            if(!(p in mapping)) {
                mapping[p] = mapping.nextPointer++;
            }
            if(p in expansion) {
                mappingFromMessage(mapping, expansion, expansion[p]);
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

function mappingFromWorkspace(mapping: {nextPointer: number} & Mapping, workspace: Workspace): void {
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
            return {tag: 'Reference', contents: mapping[msg.contents]};
        case 'Structured':
            return {tag: 'Structured', contents: msg.contents.map((m: Message) => renumberMessage(mapping, m))};
        default:
            throw "Something's wrong";
    }
}

class User {
    private pending_: Array<Either<Message, Pointer>> = [];
    private workspace_: Workspace | null = null;
    private mapping_: {nextPointer: number} & Mapping = {nextPointer: 0};
    private inverseMapping_: {[ptr: number]: Pointer} = {};

    constructor(private readonly userId: number, private readonly sessionId: number) { }

    get workspace(): Workspace | null { return this.workspace_; } // TODO: Make this an Rx-style Observable.
    get mapping(): {nextPointer: number} & Mapping { return this.mapping_; }
    get workspaceId(): number | null { const ws = this.workspace; return ws === null ? null : ws.identity; }

    private updateInverseMapping(): void {
        for(const k in this.mapping) {
            if(k === 'nextPointer') continue;
            this.inverseMapping_[this.mapping[k]] = parseInt(k, 10);
        }
    }

    private postProcess(r: Result): Result {
        switch(r.tag) {
            case 'OK':
                this.pending_ = [];
                this.workspace_ = null;
                this.mapping_ = {nextPointer: 0};
                this.inverseMapping_ = {};
                return r;
            case 'Error':
            default:
                return r;
         }
    }

    ask(msg: Message): void {
        const msg2 = renumberMessage(this.inverseMapping_, msg);
        this.pending_.push({Left: msg2});
        (<Workspace>this.workspace).subQuestions.push([null, msg2, null]);
    }

    reply(msg: Message): Promise<Result> {
        return postReply([{userId:this.userId}, <number>this.workspaceId, this.pending_, renumberMessage(this.inverseMapping_, msg)])
               .then(r => this.postProcess(r.data));
    }

    view(ptr: Pointer): Promise<Result> {
        return getPointer(ptr).then(r => {
            const msg: Message | null = r.data;
            if(msg !== null) {
                this.pending_.push({Right: ptr});
                const expansion = (<Workspace>this.workspace).expandedPointers;
                expansion[ptr] = msg;
                mappingFromMessage(this.mapping, expansion, msg);
                this.updateInverseMapping();
                return <Result>{tag: 'OK'}
            }
            return <Result>{tag: 'Error'};
        });
    }

    wait(): Promise<Result> {
        return postWait([{userId:this.userId}, <number>this.workspaceId, this.pending_]).then(r => this.postProcess(r.data));
    }

    next(): Promise<Workspace | null> {
        return postNext([{userId:this.userId}, this.sessionId]).then(response => {
            const ws = response.data;
            if(ws === null) return null;
            this.workspace_ = ws;
            mappingFromWorkspace(this.mapping, ws);
            this.updateInverseMapping();
            return ws;
        });
    }
}

const maybeSessionId = parseInt(location.hash.slice(1), 10);

(isNaN(maybeSessionId) ? getJoin() : getJoin(maybeSessionId)).then(joinResponse => {
    const userId = joinResponse.data[0].userId;
    const sessionId = joinResponse.data[1];
    location.hash = '#' + sessionId;
    const user = new User(userId, sessionId);
    workspaceDiv.addEventListener('click', evt => {
        const target = <HTMLElement | null>evt.target;
        if(target !== null && target.classList.contains('pointer')) {
            user.view(parseInt(<string>target.dataset.original, 10)).then(r => {
                if(r.tag === 'OK') {
                    renderWorkspace(user.mapping, <Workspace>user.workspace);
                } else {
                    console.log(r);
                }
            });
            evt.preventDefault();
        } else {
            // Let it propagate.
        }
    }, true);
    nextBtn.addEventListener('click', evt => {
        return user.next().then(ws => {
            if(ws === null) {
                // Do nothing but probably want to tell the user that.
            } else {
                renderWorkspace(user.mapping, ws);
                workspaceContainerDiv.style.display = 'block';
                nextBtn.style.display = 'none';
            }
        });
    });
    askBtn.addEventListener('click', evt => {
        const msg = messageParser(inputTxt.value);
        user.ask(msg);
        renderWorkspace(user.mapping, <Workspace>user.workspace);
        inputTxt.value = '';
    });
    replyBtn.addEventListener('click', evt => {
        const msg = messageParser(inputTxt.value);
        user.reply(msg).then(r => {
            if(r.tag === 'OK') {
                workspaceContainerDiv.style.display = 'none';
                nextBtn.style.display = 'inline';
                inputTxt.value = '';
            } else {
                console.log(r);
            }
        });
    });
    waitBtn.addEventListener('click', evt => {
        user.wait().then(r => {
            if(r.tag === 'OK') {
                workspaceContainerDiv.style.display = 'none';
                nextBtn.style.display = 'inline';
                inputTxt.value = '';
            } else {
                console.log(r);
            }
        });
    });
}).catch(e => console.log(e));

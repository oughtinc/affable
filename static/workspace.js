"use strict";

const workspaceDiv = document.getElementById('workspace');
const inputTxt = document.getElementById('inputTxt');
const nextBtn = document.getElementById('nextBtn');
const workspaceContainerDiv = document.getElementById('workspaceContainer');
const askBtn = document.getElementById('askBtn');
const replyBtn = document.getElementById('replyBtn');
const waitBtn = document.getElementById('waitBtn');

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

const dummy = document.createElement('textarea');
function escapeHTML(html) {
    dummy.textContent = html;
    return dummy.innerHTML;
}

function renderMessage(mapping, expansion, msg, isSubmessage) {
    switch(msg.tag) {
        case 'Text':
            return escapeHTML(msg.contents);
        case 'Reference':
            const p = msg.contents;
            if(p in expansion) {
                return renderMessage(mapping, expansion, expansion[p], true);
            } else {
                // TODO: p should *always* be in the mapping.
                return '<span class="pointer" data-original="'+p+'">$' + (p in mapping ? mapping[p] : p) + '</span>';
            }
        case 'Structured': {
            const msgs = msg.contents.map(m => renderMessage(mapping, expansion, m, true));
            return isSubmessage ? '[' + msgs.join('') + ']' : msgs.join(''); }
        case 'LabeledStructured': {
            const label = msg.contents[0];
            const msgs = msg.contents[1].map(m => renderMessage(mapping, expansion, m, true));
            // TODO: label should *always* be in the mapping.
            return '[$' + (label in mapping ? mapping[label] : label) + '|' + msgs.join('') + ']'; }
        default:
            throw "Something's wrong";
    }
}

function renderWorkspace(mapping, workspace) {
    console.log(workspace);
    const expansion = workspace.expandedPointers;
    const questionText = 'Question: ' + renderMessage(mapping, expansion, workspace.question);
    const subquestionText = workspace.subQuestions.map((q, i) => '<br/>' + (i+1) + '. ' + renderMessage(mapping, expansion, q[1])
                                                               + (q[2] !== null ? '<br/>Answer: ' + renderMessage(mapping, expansion, q[2]) : '' )).join('<br/>');

    workspaceDiv.innerHTML = questionText + '<br/>' + subquestionText + '<br/>';
}

function mappingFromMessage(mapping, expansion, msg) {
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
            msg.contents.forEach(m => mappingFromMessage(mapping, expansion, m));
            return;
        case 'LabeledStructured':
            const label = msg.contents[0];
            if(!(label in mapping)) {
                mapping[label] = mapping.nextPointer++;
            }
            msg.contents[1].forEach(m => mappingFromMessage(mapping, expansion, m));
            return;
        default:
            throw "Something's wrong";
    }
}

function mappingFromWorkspace(mapping, workspace) {
    const expansion = workspace.expandedPointers;
    mappingFromMessage(mapping, expansion, workspace.question);
    workspace.subQuestions.forEach(q => { mappingFromMessage(mapping, expansion, q[1]); if(q[2] !== null) mappingFromMessage(mapping, expansion, q[2]); });
}

function renumberMessage(mapping, msg) {
    switch(msg.tag) {
        case 'Text':
            return msg;
        case 'Reference':
            return {tag: 'Reference', contents: mapping[msg.contents]};
        case 'Structured':
            return {tag: 'Structured', contents: msg.contents.map(m => renumberMessage(mapping, m))};
        default:
            throw "Something's wrong";
    }
}

class User {
    constructor(userId) {
        this.userId = userId;
        this.pending_ = [];
        this.workspace_ = null;
        this.mapping_ = {nextPointer: 0};
        this.inverseMapping_ = {};
    }

    get workspace() { return this.workspace_; } // TODO: Make this an Rx-style Observable.
    get mapping() { return this.mapping_; }
    get workspaceId() { return this.workspace.identity; }

    postProcess(r) {
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

    ask(msg) {
        this.pending_.push(renumberMessage(this.inverseMapping_, msg));
        this.workspace.subQuestions.push([null, msg, null]);
    }

    reply(msg) {
        return postReply([{userId:this.userId}, this.workspaceId, renumberMessage(this.inverseMapping_, msg)]).then(r => this.postProcess(r.data));
    }

    view(ptr) {
        return postView([{userId:this.userId}, this.workspaceId, ptr]).then(r => this.postProcess(r.data));
    }

    wait() {
        return postWait([{userId:this.userId}, this.workspaceId, this.pending_]).then(r => this.postProcess(r.data));
    }

    next() {
        return postNext({userId:this.userId}).then(response => {
            if(response.data === null) return null;
            this.workspace_ = response.data;
            mappingFromWorkspace(this.mapping, this.workspace);
            for(const k in this.mapping) {
                if(k === 'nextPointer') continue;
                this.inverseMapping_[this.mapping[k]] = parseInt(k, 10);
            }
            return response.data;
        });
    }
}

getJoin().then(joinResponse => {
    const user = new User(joinResponse.data.userId);
    workspaceDiv.addEventListener('click', evt => {
        if(evt.target.classList.contains('pointer')) {
            user.view(parseInt(evt.target.dataset.original, 10)).then(r => {
                if(r.tag === 'OK') {
                    workspaceContainerDiv.style.display = 'none';
                    nextBtn.style.display = 'inline';
                    inputTxt.value = '';
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
        return user.next().then(r => {
            if(r === null) {
                // Do nothing but probably want to tell the user that.
            } else {
                renderWorkspace(user.mapping, user.workspace);
                workspaceContainerDiv.style.display = 'block';
                nextBtn.style.display = 'none';
            }
        });
    });
    askBtn.addEventListener('click', evt => {
        const msg = messageParser(inputTxt.value);
        user.ask(msg);
        renderWorkspace(user.mapping, user.workspace);
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

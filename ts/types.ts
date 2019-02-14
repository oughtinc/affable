import { List, Map } from 'immutable';

export type Either<A, B> = {Left: A} | {Right: B};

export type Result<A> = {tag: 'OK', contents: A} | {tag: 'Error'};

export type Pointer = number;

export type MessageTag = 'Text' | 'Reference' | 'Structured' | 'LabeledStructured';

export interface Message {
    tag: MessageTag,
    contents: any // TODO: Make more specific.
};

export type Mapping = Map<Pointer, Pointer>;

export type Expansion = Map<Pointer, Message>;

export interface Workspace {
    identity: number,
    expandedPointers: Expansion,
    question: Message,
    subQuestions: List<[null, Message, Message|null]>
};

export interface WorkspaceRaw {
    identity: number,
    expandedPointers: {[ptr: number]: Message},
    question: Message,
    subQuestions: Array<[null, Message, Message|null]>
};

export interface FetchResult<A> {
    data: A
};

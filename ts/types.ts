import { List, Map } from 'immutable';

export type Either<A, B> = {Left: A} | {Right: B};

export type Result<A> = {tag: 'OK', contents: A} | {tag: 'Error'};

export type Pointer = number;

export type MessageTag = 'Text' | 'Reference' | 'Structured' | 'LabeledStructured';

export interface TextMessage {
    tag: 'Text',
    contents: string
};

export interface ReferenceMessage {
    tag: 'Reference',
    contents: number
};

export interface StructuredMessage {
    tag: 'Structured',
    contents: Array<Message>
};

export interface LabeledStructuredMessage {
    tag: 'LabeledStructured',
    contents: [number, Array<Message>]
};

export type Message = TextMessage | ReferenceMessage | StructuredMessage | LabeledStructuredMessage;

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

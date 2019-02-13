export type Either<A, B> = {Left: A} | {Right: B};

export type Result = {tag: 'OK'|'Error'};

export type Pointer = number;
export type MessageTag = 'Text' | 'Reference' | 'Structured' | 'LabeledStructured';
export type Message = {tag: MessageTag, contents: any}; // TODO: Make more specific.
export type Mapping = {[ptr: number]: Pointer};
export type Expansion = {[ptr: number]: Message};
export type Workspace = {identity: number, expandedPointers: Expansion, question: Message, subQuestions: Array<[null, Message, Message|null]>};

export type FetchResult<A> = {data: A};

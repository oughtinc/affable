# Current design notes

Core is conceptually a concurrent system.
    
Automation is a wrapper around concurrent processes.
    
The scheduler is literally the scheduler of the processes.
    
Considering using a relational representation, or even a relational database,
for the system state. I want to avoid state being hidden in closures. This  is
also useful for history, monitoring, and extracting Q&A pairs, say, for
distillation. (Considering whether ixset/ixset-typed library would suffice.)

Caching should be doable by automation. The biggest issue is sharing state
globally. This can be accomplished by adding a special process or in a more
direct manner. Basically, the wrapping automation would accumulate the actions
that are being performed by the wrapped process and submit those to the caching
process. I'm not confident that there aren't some more gotchas, but if so,
implementing caching at a deeper level should be reasonably straightforward.

I think I can arrange things so that there is an abstract logical time concept
and varying it will lead to different semantics. For example, a constant time
should lead to every process having identity, while a vector clock would lead
to causally independent messages to the "same" process producing independent
processes. Another possibility is that logical time is the full history
of received messages (and other actions, likely). I sometimes refer to this 
approach as "content-based addressing", though maybe "history-based addressing"
would be more sensible.

I'm going to see how it goes having the scratchpad be handled with
self-messages. Adding explicit support for the scratchpad should be
straightforward if necessary. There will likely be some special-casing in the
user-interface to present a scratchpad-like interface even if it is just
self-messages underneath.

Vague ideas on supporting intercession on scheduling. Essentially, the
scheduler can start a new process with the question about what to schedule next
and schedule those with its old self.

## Why have answers rather than just sending messages back to the creator

Basically, whenever I try to eliminate the concept of an "answer" as a distinct
concept I run into issues where it seems you need to treat answers differently.

For example, for some versions of the multi-version workspaces (based on the notion
of logical time), an answer would go back to the version that asked the original
question, while a message might go to a version which you've been in dialog with.
Or an answer may indicate the cache should be updated.

Answering also keeps the "parent" workspace implicit which, naively, lessens
questions being different only on who is asking. That said, if the "parent" address
was behind a pointer or something, maybe that would happen automatically so this
wouldn't be an issue.

## The use of a relational database

One of the purposes for having a relational model stored in a relational database
is to force system state to be explicit and accessible.

I also think it will be convenient for both implementation and after-action evaluation.

## Caching

I had the intuition that with the content-based addressing caching might become
trivial, but we would need to have the messages be identified modulo pointers. That
said, I'm not completely sure that that is problematic, if expanding a pointer is a
step that leads to a new version.

With caching, there is the issue of a cached wrong answer. Something is needed,
or at least desirable, to be able to deal with this. Editing, of some form, may
allow this to be resolved. Alternatively, one could imagine some kind of uncaching
(which would be too implementation-oriented) or, less implementation-oriented, an
explicit "re-ask". This starts to lead to similar kind of abilities to "reject"
an answer in a more explicit way rather than just ignoring it and asking another
question. In this case, rejecting an answer would lead to similar kind of "staleness"
issues as editing. You'd probably want to incorporate something about the rejected
answer, so you don't just get the same rejected answer again. At that point, though,
it becomes similar to just asking a new question that is the old question plus some
context on the rejected answer.


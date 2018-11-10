Topics to explore:
    - Framework that allows automation based on actions or whatever is appropriate.
    - Framework that allows continuous distillation
    - Probably support reflection
    - Intercession on scheduling
        - Ideally some support for distillation of scheduling.
    - Internal dialog via message passing
        - Persistent processes
        - Auto-replicating processes
            - Only access to the causally "latest" (from the current process' perspective) node
            - or acess to earlier "versions" as well
    - Or, editing versus internal dialog
        - How to handle staleness?
        - Should editing be non-local? If no, what does that mean exactly?
    
    - Intercession on other aspects (Rendering, Distillation, Automation?)

Current design notes:

Core is a concurrent system.
    
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
processes.

I'm going to see how it goes having the scratchpad be handled with
self-messages. Adding explicit support for the scratchpad should be
straightforward if necessary. There will likely be some special-casing in the
user-interface to present a scratchpad-like interface even if it is just
self-messages underneath.

Vague ideas on supporting intercession on scheduling. Essentially, the
scheduler can start a new process with the question about what to schedule next
and schedule those with its old self.

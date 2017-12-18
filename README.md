`React.Queue.LifeCycle` gives a react component write access to a queue, informing it of the component's current life cycle, as
a data type.

`React.Queue.WhileMounted` gives an event handler read access to a queue, when inside a component's normal "mounted" status.
This is useful for binding a short-lived component to a long-lived data channel.

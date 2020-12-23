module CircularLinkedLists

export CircularLinkedList

mutable struct CircularLinkedList{T}
    value::T
    next::Union{CircularLinkedList, Nothing}
    prev::Union{CircularLinkedList, Nothing}
end

function CircularLinkedList{T}(values::Vector{T}) where T
    if isempty(values)
        throw("Can't construct an empty linked list!")
    end

    nodes = map(values) do value
        CircularLinkedList{T}(value, nothing, nothing)
    end

    for i in 2:length(nodes)-1
        node = nodes[i]
        node.prev = nodes[i - 1]
        node.next = nodes[i + 1]
        node.prev.next = node
        node.next.prev = node
    end

    nodes[1].prev = nodes[end]
    nodes[end].next = nodes[1]

    return nodes[1]
end

function Base.delete!(item::CircularLinkedList{T}) where T
    item.prev.next = item.next
    item.next.prev = item.prev
    item.next = item.prev = nothing
    return item
end

function Base.insert!(before::CircularLinkedList{T}, item::CircularLinkedList{T}) where T
    after = before.next
    after.prev = item
    before.next = item
    item.next = after
    item.prev = before
end

function Base.collect(list::CircularLinkedList{T})::Vector{T} where T
    values = [list.value]
    start = list
    curr = list.next

    while curr !== start
        push!(values, curr.value)
        curr = curr.next
    end

    values
end

function Base.show(io::IO, list::CircularLinkedList{T}) where T
    curr = list

    while true
        print(curr.value)
        curr = curr.next
        if curr === nothing || curr === list
            break
        end
        print(io, ", ")
    end
end

function Base.iterate(list::CircularLinkedList{T}, state=(list, list, false)) where T
    curr, start, loop = state

    if curr === start && loop
        return nothing
    else
        loop = true
    end

    return (curr, (curr.next, start, loop))
end

end

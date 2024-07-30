module internal EmbassyAccess.Worker.Countries.Slovenia

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Ljubljana =
    Node({ Name = "Ljubljana"; Handle = None }, [ Russian.createNode <| Slovenia Ljubljana ])

let Node = Node({ Name = "Slovenia"; Handle = None }, [ Ljubljana ])
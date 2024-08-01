module internal EmbassyAccess.Worker.Countries.Slovenia

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Ljubljana =
    Graph.Node({ Name = "Ljubljana"; Handle = None }, [ Russian.createNode <| Slovenia Ljubljana ])

let Node = Graph.Node({ Name = "Slovenia"; Handle = None }, [ Ljubljana ])
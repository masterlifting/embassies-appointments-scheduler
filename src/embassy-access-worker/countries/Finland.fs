module internal EmbassyAccess.Worker.Countries.Finland

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Helsinki =
    Graph.Node({ Name = "Helsinki"; Handle = None }, [ Russian.createNode <| Finland Helsinki ])

let Node = Graph.Node({ Name = "Finland"; Handle = None }, [ Helsinki ])
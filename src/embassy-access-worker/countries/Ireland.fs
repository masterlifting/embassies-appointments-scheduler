module internal EmbassyAccess.Worker.Countries.Ireland

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Dublin =
    Graph.Node({ Name = "Dublin"; Handle = None }, [ Russian.createNode <| Ireland Dublin ])

let Node = Graph.Node({ Name = "Ireland"; Handle = None }, [ Dublin ])
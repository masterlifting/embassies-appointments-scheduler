module internal EmbassyAccess.Worker.Countries.Albania

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Tirana =
    Graph.Node({ Name = "Tirana"; Handle = None }, [ Russian.createNode <| Albania Tirana ])

let Node = Graph.Node({ Name = "Albania"; Handle = None }, [ Tirana ])
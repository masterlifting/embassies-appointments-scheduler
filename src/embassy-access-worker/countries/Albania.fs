module internal EmbassyAccess.Worker.Countries.Albania

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Tirana =
    Node({ Name = "Tirana"; Handle = None }, [ Russian.createNode <| Albania Tirana ])

let Node = Node({ Name = "Albania"; Handle = None }, [ Tirana ])
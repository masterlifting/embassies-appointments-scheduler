module internal EmbassyAccess.Worker.Countries.Switzerland

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Bern =
    Graph.Node({ Name = "Bern"; Handle = None }, [ Russian.createNode <| Switzerland Bern ])

let Node = Graph.Node({ Name = "Switzerland"; Handle = None }, [ Bern ])
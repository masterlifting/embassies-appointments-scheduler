module internal EmbassyAccess.Worker.Countries.Switzerland

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Bern =
    Node({ Name = "Bern"; Handle = None }, [ Russian.createNode <| Switzerland Bern ])

let Node = Node({ Name = "Switzerland"; Handle = None }, [ Bern ])
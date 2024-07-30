module internal EmbassyAccess.Worker.Countries.Hungary

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Budapest =
    Node({ Name = "Budapest"; Handle = None }, [ Russian.createNode <| Hungary Budapest ])

let Node = Node({ Name = "Hungary"; Handle = None }, [ Budapest ])
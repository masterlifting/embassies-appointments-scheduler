module internal EmbassyAccess.Worker.Countries.Hungary

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Budapest =
    Graph.Node({ Name = "Budapest"; Handle = None }, [ Russian.createNode <| Hungary Budapest ])

let Node = Graph.Node({ Name = "Hungary"; Handle = None }, [ Budapest ])
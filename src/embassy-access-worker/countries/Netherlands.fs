module internal EmbassyAccess.Worker.Countries.Netherlands

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Hague =
    Graph.Node({ Name = "Hague"; Handle = None }, [ Russian.createNode <| Netherlands Hague ])

let Node = Graph.Node({ Name = "Netherlands"; Handle = None }, [ Hague ])
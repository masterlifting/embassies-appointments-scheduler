module internal EmbassyAccess.Worker.Countries.Netherlands

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Hague =
    Node({ Name = "Hague"; Handle = None }, [ Russian.createNode <| Netherlands Hague ])

let Node = Node({ Name = "Netherlands"; Handle = None }, [ Hague ])
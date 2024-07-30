module internal EmbassyAccess.Worker.Countries.Finland

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Helsinki =
    Node({ Name = "Helsinki"; Handle = None }, [ Russian.createNode <| Finland Helsinki ])

let Node = Node({ Name = "Finland"; Handle = None }, [ Helsinki ])
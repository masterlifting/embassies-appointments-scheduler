module internal EmbassyAccess.Worker.Countries.Ireland

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Dublin =
    Node({ Name = "Dublin"; Handle = None }, [ Russian.createNode <| Ireland Dublin ])

let Node = Node({ Name = "Ireland"; Handle = None }, [ Dublin ])
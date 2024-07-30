module internal EmbassyAccess.Worker.Countries.France

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Paris =
    Node({ Name = "Paris"; Handle = None }, [ Russian.createNode <| France Paris ])

let Node = Node({ Name = "France"; Handle = None }, [ Paris ])
module internal EmbassyAccess.Worker.Countries.France

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Paris =
    Graph.Node({ Name = "Paris"; Handle = None }, [ Russian.createNode <| France Paris ])

let Node = Graph.Node({ Name = "France"; Handle = None }, [ Paris ])
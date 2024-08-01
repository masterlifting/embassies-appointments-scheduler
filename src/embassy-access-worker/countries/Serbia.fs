module internal EmbassyAccess.Worker.Countries.Serbia

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Belgrade =
    Graph.Node({ Name = "Belgrade"; Handle = None }, [ Russian.createNode <| Serbia Belgrade ])

let Node = Graph.Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])
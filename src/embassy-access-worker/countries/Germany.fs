module internal EmbassyAccess.Worker.Countries.Germany

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Berlin =
    Graph.Node({ Name = "Berlin"; Handle = None }, [ Russian.createNode <| Germany Berlin ])

let Node = Graph.Node({ Name = "Germany"; Handle = None }, [ Berlin ])
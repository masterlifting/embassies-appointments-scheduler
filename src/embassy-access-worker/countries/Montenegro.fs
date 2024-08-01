module internal EmbassyAccess.Worker.Countries.Montenegro

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Podgorica =
    Graph.Node({ Name = "Podgorica"; Handle = None }, [ Russian.createNode <| Montenegro Podgorica ])

let Node = Graph.Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])
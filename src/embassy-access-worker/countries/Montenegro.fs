module internal EmbassyAccess.Worker.Countries.Montenegro

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Podgorica =
    Node({ Name = "Podgorica"; Handle = None }, [ Russian.createNode <| Montenegro Podgorica ])

let Node = Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])
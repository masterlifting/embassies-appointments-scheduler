module internal EmbassyAccess.Worker.Countries.Germany

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Berlin =
    Node({ Name = "Berlin"; Handle = None }, [ Russian.createNode <| Germany Berlin ])

let Node = Node({ Name = "Germany"; Handle = None }, [ Berlin ])
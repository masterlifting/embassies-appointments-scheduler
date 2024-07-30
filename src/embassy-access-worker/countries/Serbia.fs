module internal EmbassyAccess.Worker.Countries.Serbia

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Belgrade =
    Node({ Name = "Belgrade"; Handle = None }, [ Russian.createNode <| Serbia Belgrade ])

let Node = Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])
module internal EmbassyAccess.Worker.Countries.Bosnia

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Worker.Embassies

let private Sarajevo =
    Node({ Name = "Sarajevo"; Handle = None }, [ Russian.createNode <| Bosnia Sarajevo ])

let Node = Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])
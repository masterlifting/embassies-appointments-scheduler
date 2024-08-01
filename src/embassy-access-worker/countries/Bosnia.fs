module internal EmbassyAccess.Worker.Countries.Bosnia

open Infrastructure.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Worker.Embassies

let private Sarajevo =
    Graph.Node({ Name = "Sarajevo"; Handle = None }, [ Russian.createNode <| Bosnia Sarajevo ])

let Node = Graph.Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])
module internal Eas.Worker.Countries

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open Eas.Domain.Internal

module Serbia =
    let private Belgrade =
        Node({ Name = "Belgrade"; Handle = None }, [ Embassies.Russian.createNode <| Serbia Belgrade ])

    let Node = Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])

module Bosnia =
    let private Sarajevo =
        Node({ Name = "Sarajevo"; Handle = None }, [ Embassies.Russian.createNode <| Bosnia Sarajevo ])

    let Node = Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])

module Montenegro =
    let private Podgorica =
        Node({ Name = "Podgorica"; Handle = None }, [ Embassies.Russian.createNode <| Montenegro Podgorica ])

    let Node = Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])

module Albania =
    let private Tirana =
        Node({ Name = "Tirana"; Handle = None }, [ Embassies.Russian.createNode <| Albania Tirana ])

    let Node = Node({ Name = "Albania"; Handle = None }, [ Tirana ])

module Hungary =
    let private Budapest =
        Node({ Name = "Budapest"; Handle = None }, [ Embassies.Russian.createNode <| Hungary Budapest ])

    let Node = Node({ Name = "Hungary"; Handle = None }, [ Budapest ])

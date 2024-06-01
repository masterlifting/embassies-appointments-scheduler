module internal Eas.Worker.Core.Countries

open Infrastructure.Domain.Graph
open Worker.Domain.Core
open Eas.Domain.Core

module Serbia =
    let private Belgrade =
        Node({ Name = "Belgrade"; Handle = None }, [ Embassies.Russian.createStepsFor Belgrade ])

    let Handler = Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])

module Bosnia =
    let private Sarajevo =
        Node({ Name = "Sarajevo"; Handle = None }, [ Embassies.Russian.createStepsFor Sarajevo ])

    let Handler = Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])

module Montenegro =
    let private Podgorica =
        Node({ Name = "Podgorica"; Handle = None }, [ Embassies.Russian.createStepsFor Podgorica ])

    let Handler = Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])

module Albania =
    let private Tirana =
        Node({ Name = "Tirana"; Handle = None }, [ Embassies.Russian.createStepsFor Tirana ])

    let Handler = Node({ Name = "Albania"; Handle = None }, [ Tirana ])

module Hungary =
    let private Budapest =
        Node({ Name = "Budapest"; Handle = None }, [ Embassies.Russian.createStepsFor Budapest ])

    let Handler = Node({ Name = "Hungary"; Handle = None }, [ Budapest ])

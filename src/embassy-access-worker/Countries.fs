module internal EmbassyAccess.Worker.Countries

open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Domain.Core.Internal

module Albania =
    let private Tirana =
        Node({ Name = "Tirana"; Handle = None }, [ Embassies.Russian.createNode <| Albania Tirana ])

    let Node = Node({ Name = "Albania"; Handle = None }, [ Tirana ])

module Bosnia =
    let private Sarajevo =
        Node({ Name = "Sarajevo"; Handle = None }, [ Embassies.Russian.createNode <| Bosnia Sarajevo ])

    let Node = Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])

module Finland =
    let private Helsinki =
        Node({ Name = "Helsinki"; Handle = None }, [ Embassies.Russian.createNode <| Finland Helsinki ])

    let Node = Node({ Name = "Finland"; Handle = None }, [ Helsinki ])

module France =
    let private Paris =
        Node({ Name = "Paris"; Handle = None }, [ Embassies.Russian.createNode <| France Paris ])

    let Node = Node({ Name = "France"; Handle = None }, [ Paris ])

module Germany =
    let private Berlin =
        Node({ Name = "Berlin"; Handle = None }, [ Embassies.Russian.createNode <| Germany Berlin ])

    let Node = Node({ Name = "Germany"; Handle = None }, [ Berlin ])

module Hungary =
    let private Budapest =
        Node({ Name = "Budapest"; Handle = None }, [ Embassies.Russian.createNode <| Hungary Budapest ])

    let Node = Node({ Name = "Hungary"; Handle = None }, [ Budapest ])

module Ireland =
    let private Dublin =
        Node({ Name = "Dublin"; Handle = None }, [ Embassies.Russian.createNode <| Ireland Dublin ])

    let Node = Node({ Name = "Ireland"; Handle = None }, [ Dublin ])

module Montenegro =
    let private Podgorica =
        Node({ Name = "Podgorica"; Handle = None }, [ Embassies.Russian.createNode <| Montenegro Podgorica ])

    let Node = Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])

module Netherlands =
    let private Hague =
        Node({ Name = "Hague"; Handle = None }, [ Embassies.Russian.createNode <| Netherlands Hague ])

    let Node = Node({ Name = "Netherlands"; Handle = None }, [ Hague ])

module Serbia =
    let private Belgrade =
        Node({ Name = "Belgrade"; Handle = None }, [ Embassies.Russian.createNode <| Serbia Belgrade ])

    let Node = Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])

module Slovenia =
    let private Ljubljana =
        Node({ Name = "Ljubljana"; Handle = None }, [ Embassies.Russian.createNode <| Slovenia Ljubljana ])

    let Node = Node({ Name = "Slovenia"; Handle = None }, [ Ljubljana ])

module Switzerland =
    let private Bern =
        Node({ Name = "Bern"; Handle = None }, [ Embassies.Russian.createNode <| Switzerland Bern ])

    let Node = Node({ Name = "Switzerland"; Handle = None }, [ Bern ])

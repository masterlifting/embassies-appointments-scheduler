module EmbassyAccess.Domain

open System

module Internal =

    type RequestId =
        | RequestId of Guid

        member this.Value =
            match this with
            | RequestId id -> id

    type ResponseId =
        | ResponseId of Guid

        member this.Value =
            match this with
            | ResponseId id -> id

    type AppointmentId =
        | AppointmentId of Guid

        member this.Value =
            match this with
            | AppointmentId id -> id

    type City =
        | Belgrade
        | Berlin
        | Budapest
        | Sarajevo
        | Podgorica
        | Tirana
        | Paris
        | Rome
        | Dublin
        | Bern
        | Helsinki
        | Hague
        | Ljubljana

    type Country =
        | Serbia of City
        | Germany of City
        | Bosnia of City
        | Montenegro of City
        | Albania of City
        | Hungary of City
        | Ireland of City
        | Switzerland of City
        | Finland of City
        | France of City
        | Netherlands of City
        | Slovenia of City

    type Embassy =
        | Russian of Country
        | Spanish of Country
        | Italian of Country
        | French of Country
        | German of Country
        | British of Country

    type Appointment =
        { Id: AppointmentId
          Value: string
          Date: DateOnly
          Time: TimeOnly
          Description: string option }

    type ConfirmationOption =
        | FirstAvailable
        | Range of DateTime * DateTime
        | Appointment of Appointment

    type Request =
        { Id: RequestId
          Value: string
          Attempt: int
          Embassy: Embassy
          Modified: DateTime }

    type AppointmentsResponse =
        { Id: ResponseId
          Request: Request
          Appointments: Set<Appointment>
          Modified: DateTime }

    type ConfirmationResponse =
        { Id: ResponseId
          Request: Request
          Description: string
          Modified: DateTime }

module External =

    type City() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set

    type Country() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set
        member val CityId: int = 0 with get, set
        member val City: City = City() with get, set

    type Embassy() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set
        member val CountryId: int = 0 with get, set
        member val Country: Country = Country() with get, set

    type Request() =
        member val Id: Guid = Guid.Empty with get, set
        member val Value: string = String.Empty with get, set
        member val Attempt: int = 0 with get, set
        member val UserId: int = 0 with get, set
        member val EmbassyId: int = 0 with get, set
        member val Embassy: Embassy = Embassy() with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    type AppointmentsResponse() =
        member val Id: Guid = Guid.Empty with get, set
        member val RequestId: Guid = Guid.Empty with get, set
        member val Request: Request = Request() with get, set
        member val Appointments: Appointment array = [||] with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    and Appointment() =
        member val Id: Guid = Guid.Empty with get, set
        member val Value: string = String.Empty with get, set
        member val DateTime: DateTime = DateTime.UtcNow with get, set
        member val Description: string = String.Empty with get, set

    type ConfirmationResponse() =
        member val Id: Guid = Guid.Empty with get, set
        member val RequestId: Guid = Guid.Empty with get, set
        member val Request: Request = Request() with get, set
        member val Description: string = String.Empty with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

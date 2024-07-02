module internal Eas.Worker.Embassies

open Infrastructure.DSL
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence.Filter
open Persistence.Domain

module Russian =
    open Web.Client
    open Eas.Persistence

    let private getRequests ct country storage =
        let filter =
            Request.ByEmbassy(
                { Pagination =
                    { Page = 1
                      PageSize = 5
                      SortBy = Desc(Date(_.Modified)) }
                  Embassy = Russian country }
            )

        storage |> Repository.Query.Request.get ct filter

    let private tryGetResponse ct storage requests =

        let getResponse =
            Eas.Core.Russian.getResponse
                { getStartPage = Http.Request.Get.string ct
                  postValidationPage = Http.Request.Post.waitString ct
                  getCaptchaImage = Http.Request.Get.bytes ct
                  solveCaptchaImage = Http.Captcha.AntiCaptcha.solveToInt ct }

        let updateRequest request =
            storage |> Repository.Command.Request.update ct request

        requests
        |> Eas.Core.Russian.tryGetResponse
            { updateRequest = updateRequest
              getResponse = getResponse }

    let private handleResponse ct storage response =

        let saveResponse response =
            storage |> Repository.Command.Response.create ct response

        match response with
        | None -> async { return Ok <| Info "No data." }
        | Some response ->
            response
            |> saveResponse
            |> ResultAsync.map (fun _ -> Success response.Appointments)


    let private searchAppointments country =
        fun _ ct ->
            Persistence.Core.createStorage InMemory
            |> ResultAsync.wrap (fun storage ->
                storage
                |> getRequests ct country
                |> ResultAsync.bind' (tryGetResponse ct storage)
                |> ResultAsync.bind' (handleResponse ct storage))

    let createNode country =
        Node(
            { Name = "Russian"; Handle = None },
            [ Node(
                  { Name = "Search Appointments"
                    Handle = Some <| searchAppointments country },
                  []
              ) ]
        )

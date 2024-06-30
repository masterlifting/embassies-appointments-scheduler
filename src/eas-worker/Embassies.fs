module internal Eas.Worker.Embassies

open Infrastructure.Dsl
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence.Filter
open Persistence.Domain

module Russian =
    open Web.Client
    open Eas.Persistence

    let private getRequests country ct storage =
        let filter =
            Request.ByEmbassy(
                { Pagination =
                    { Page = 1
                      PageSize = 5
                      SortBy = Desc(Date(_.Modified)) }
                  Embassy = Russian country }
            )

        storage |> Repository.Query.Request.get filter ct

    let private tryGetResponse storage ct requests =

        let getResponse request =
            let getResponse' props =
                Eas.Core.Russian.getResponse props request ct

            getResponse'
                { getStartPage = Http.Request.Get.string ct
                  postValidationPage = Http.Request.Post.waitString ct
                  getCaptchaImage = Http.Request.Get.bytes ct
                  solveCaptchaImage = Http.Captcha.AntiCaptcha.solveToInt ct}

        let updateRequest request =
            storage |> Repository.Command.Request.update request ct

        requests
        |> Eas.Core.Russian.tryGetResponse
            { updateRequest = updateRequest
              getResponse = getResponse }

    let private handleResponse storage ct response =

        let saveResponse response =
            storage |> Repository.Command.Response.create response ct

        match response with
        | None -> async { return Ok <| Info "No data." }
        | Some response ->
            response
            |> saveResponse
            |> ResultAsync.map (fun _ -> Success response.Appointments)


    let private lookForApointments country =
        fun _ ct ->
            Persistence.Core.createStorage InMemory
            |> ResultAsync.wrap (fun storage ->
                storage
                |> getRequests country ct
                |> ResultAsync.bind' (tryGetResponse storage ct)
                |> ResultAsync.bind' (handleResponse storage ct))

    let createNode country =
        Node(
            { Name = "Russian"; Handle = None },
            [ Node(
                  { Name = "Look for appointments"
                    Handle = Some <| lookForApointments country },
                  []
              ) ]
        )

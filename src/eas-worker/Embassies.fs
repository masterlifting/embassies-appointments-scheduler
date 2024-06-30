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

    let private getRequest country ct storage =
        let filter =
            Request.ByEmbassy(
                { Pagination =
                    { Page = 1
                      PageSize = 5
                      SortBy = Desc(Date(_.Modified)) }
                  Embassy = Russian country }
            )

        storage |> Repository.Query.Request.get filter ct

    let private getResponse request ct =
        let get props =
            Eas.Core.Russian.getResponse props request ct

        get
            { getStartPage = Http.Request.Get.string
              postValidationPage = Http.Request.Post.waitString
              getCaptchaImage = Http.Request.Get.bytes
              solveCaptchaImage = Http.Captcha.AntiCaptcha.solveToInt }

    let private getAvailableDates country =
        fun _ ct ->
            Persistence.Core.createStorage InMemory
            |> ResultAsync.wrap (fun storage ->

                let updateRequest request =
                    storage |> Repository.Command.Request.update request ct

                // let getResponse request =
                //     let get props =
                //         Eas.Core.Russian.getResponse props request ct

                //     get
                //         { getStartPage = Http.Request.Get.string
                //           postValidationPage = Http.Request.Post.waitString
                //           getCaptchaImage = Http.Request.Get.bytes
                //           solveCaptchaImage = Http.Captcha.AntiCaptcha.solveToInt }

                let tryGetResponse requests =
                    Eas.Core.Russian.tryGetResponse requests updateRequest getResponse

                let saveResponse response =
                    storage |> Repository.Command.Response.create response ct

                let handleResponse response =
                    match response with
                    | None -> async { return Ok <| Info "No data." }
                    | Some response ->
                        response
                        |> saveResponse
                        |> ResultAsync.map (fun _ -> Success response.Appointments)

                storage
                |> getRequest country ct
                |> ResultAsync.bind' tryGetResponse
                |> ResultAsync.bind' handleResponse)

    let createNode country =
        Node(
            { Name = "Russian"; Handle = None },
            [ Node(
                  { Name = "Look for appointments"
                    Handle = Some <| getAvailableDates country },
                  []
              ) ]
        )

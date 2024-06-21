module Eas.WebClient

open Infrastructure.Dsl
open Infrastructure.Domain.Errors
open Web.Domain

module Http =
    open Web

    let send (client: Http.Client) (request: Http.Domain.Request) ct =
        match request with
        | Http.Domain.Request.Get(path, headers) -> Http.get client path headers ct
        | Http.Domain.Request.Post(path, data, headers) -> Http.post client path data headers ct

module Telegram =
    open Web

    let send (client: Telegram.Client) (request: Telegram.Domain.Internal.Request) ct =
        match request with
        | Telegram.Domain.Internal.Request.Message(chatId, text) -> Telegram.sendText chatId text ct
        | _ -> async { return Error(Logical(NotSupported $"{client}")) }

module Repository =
    let send client request ct =
        match client, request with
        | HttpClient client, Request.Http request -> Http.send client request ct |> ResultAsync.map Http
        | TelegramClient client, Request.Telegram request -> Telegram.send client request ct |> ResultAsync.map Telegram
        | _ -> async { return Error(Logical(NotSupported $"{client} or {request}")) }

    let send'<'a> client request ct =
        match client, request with
        | HttpClient client, Request.Http request ->
            Http.send client request ct
            |> ResultAsync.bind (fun response ->
                let content, _ = response
                SerDe.Json.deserialize<'a> content |> Result.mapError Infrastructure)
        | TelegramClient client, Request.Telegram request ->
            Telegram.send client request ct
            |> ResultAsync.bind (fun response -> SerDe.Json.deserialize<'a> response |> Result.mapError Infrastructure)
        | _ -> async { return Error(Logical(NotSupported $"{client} or {request}")) }

    let receive client request ct =
        match client with
        | _ -> async { return Error(Logical(NotSupported $"{client}")) }

module Parser =
    module Html =
        open HtmlAgilityPack
        open Infrastructure.Dsl.ActivePatterns

        let private hasError (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectSingleNode("//div[@class='error_msg']") with
                | null -> Ok html
                | error ->
                    match error.InnerText with
                    | IsString msg -> Error(Logical(Business msg))
                    | _ -> Ok html
            with ex ->
                Error(Infrastructure(Parsing ex.Message))

        let private getNode (xpath: string) (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectSingleNode(xpath) with
                | null -> Error(Logical(Business "Node not found"))
                | node -> Ok node
            with ex ->
                Error(Infrastructure(Parsing ex.Message))

        let private getNodes (xpath: string) (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectNodes(xpath) with
                | null -> Error(Logical(Business "Nodes not found"))
                | nodes -> Ok nodes
            with ex ->
                Error(Infrastructure(Parsing ex.Message))

        let private getAttributeValue (attribute: string) (node: HtmlNode) =
            try
                match node.GetAttributeValue(attribute, "") with
                | "" -> Error(Logical(Business "Attribute not found"))
                | value -> Ok value
            with ex ->
                Error(Infrastructure(Parsing ex.Message))
        
        let test = """
            <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

            <html xmlns="http://www.w3.org/1999/xhtml" >
            <head><title>
                Система электронной записи на прием
            </title><link rel="icon" href="/favicon.ico" type="image/x-icon" /><link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" /><link rel="stylesheet" type="text/css" href="css/Styles.css" /><link rel="stylesheet" type="text/css" href="css/StyleSheet.css" />
                <link rel="stylesheet" type="text/css" href="css/feedback.css" />
                <script type="text/javascript">
                <!--
                    function onClickButton() {
                    var item = document.getElementById("wait_answer");
                    item.style.visibility = "visible";
                }
                //-->
                </script>

            </head>
            <body>
            <div id="body" >
                <div id="top"> 
                    <span>
                        

                    </span>
                </div>

            <div id="hdr"> 
                <div id="hdr2">
                        <div class="gerb"></div>
                        <span class=header>
                            <div class="text">Консульский отдел<br>Посольства Российской Федерации <br> в Боснии и Герцеговине (Сараево)</div>
                            <div class="text">
                                <span style="font-size:16px; color:#fff;">Система электронной записи на прием</span>
                            </div>
                        </span>             
                </div>
            </div>
                
                
            <div id="desk">
                        
            <form name="aspnetForm" method="post" action="orderinfo.aspx?id=20781&amp;cd=f23cb539&amp;ems=143F4DDF" id="aspnetForm" onsubmit="WaitAnswer()">
            <input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="/wEPDwUKMTcyNjMyOTQ4Nw9kFgJmD2QWAgIFD2QWAgIBDxYCHghvbnN1Ym1pdAUMV2FpdEFuc3dlcigpFgICAw9kFgQCBg8PFgIeDEVycm9yTWVzc2FnZQUMRXJyb3JNZXNzYWdlZGQCEg8PFgIeCEltYWdlVXJsBRh+L0NvZGVJbWFnZS5hc3B4P2lkPWM2MjBkZGQ1syQhvYDrXtXsobHB1L4T+3NBXg==" />

            <input type="hidden" name="__VIEWSTATEGENERATOR" id="__VIEWSTATEGENERATOR" value="EE4D9765" />
            <input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="/wEWBwLpis68CgLmjdfGDQKLs7ufCwK5ysLjCwKj8MqYCAKyrYjZCwLUs+euCxXMvf5Hj8iJu5XvmPV7R4IIZDIe" />

            <table><tr><td id="left-panel">
                    
            <div class="box_instruction"   style="display:none;"><p>Заполните поля информацией, полученной при оформлении записи, из распечатки «Подтверждения записи на прием», нажмите кнопку «Далее».</p></div>

            <div class="box_instruction"  style="display:none;"><p>Для проверки наличия свободного времени для записи на прием нажмите на кнопку «Записаться на прием»</p></div>
            </td>
            <td id="center-panel">
            
                <script>
                document.getElementsByClassName('box_instruction')[0].style.display = '';
                document.getElementsByClassName('box_instruction')[1].style.display = 'none';
            </script>
                <h1>
                    <span id="ctl00_MainContent_Label_Header">ИНФОРМАЦИЯ О ЗАЯВКЕ</span>
                </h1>
                <br />
                <p>
                Номер заявки
                </p>
                <div class="inp" style="margin-left:20px;">
                <input name="ctl00$MainContent$txtID" type="text" value="20781" id="ctl00_MainContent_txtID" />
                </div>
                <p> 
                Защитный код
                </p>
                <div class="inp" style="margin-left:20px;">
                <input name="ctl00$MainContent$txtUniqueID" type="text" value="F23CB539" id="ctl00_MainContent_txtUniqueID" />
                        
                        
                        
                <p>
                
                </p>
                </div>
                <!-- -->
            <style>#ImgCnt{overflow: hidden;width: 200px;height: 200px; margin: 0 0 20px;}#ImgCnt img{margin: 0px 0px 0px -200px;}</style>
                <div class="capcha-title">
                Введите символы с картинки.
                </div>
                <div class="inp">    
                    <div id='ImgCnt'> 
                            <img id="ctl00_MainContent_imgSecNum" src="CodeImage.aspx?id=c620" alt="Необходимо включить загрузку картинок в браузере." border="0" />
                    </div>
                <input name="ctl00$MainContent$txtCode" type="text" id="ctl00_MainContent_txtCode" onpaste="return false" />
                </div>
            
                <!-- -->
                <div>
                
                </div>
            <input type="submit" name="ctl00$MainContent$ButtonA" value="Далее" onclick="javascript:WebForm_DoPostBackWithOptions(new WebForm_PostBackOptions(&quot;ctl00$MainContent$ButtonA&quot;, &quot;&quot;, true, &quot;&quot;, &quot;&quot;, false, false))" id="ctl00_MainContent_ButtonA" class="btn" />
            
            
            
                <div>
                
                </div>
            </td></tr></table>


            <input type="hidden" name="ctl00$MainContent$FeedbackClientID" id="ctl00_MainContent_FeedbackClientID" value="0" />
            <input type="hidden" name="ctl00$MainContent$FeedbackOrderID" id="ctl00_MainContent_FeedbackOrderID" value="0" />


            </form>
            <div id="wait_answer"  >
            <p> 
            </p></div>
            <script>

            if(document.getElementsByClassName('inp').length==0)
            {
            document.getElementsByClassName('box_instruction')[0].style.display='none';
            }
            </script>

                <div style="clear:both;"></div>
                </div>    
                <div id="footer">
                    <div class="rightfooter" >


                        <p>© Сайт КД МИД России "Запись на прием в КЗУ" <br/>2022</p>
                    </div>
                    <div style="clear:both;"></div>
                </div>    
            </div> 
            <div id="wait_answer">
                <div id="wait_answer_img">
                    <p style="margin-top: 50px; margin-left: -26px;">
                        <span id="wait_answer_span" style="color:White;">Загрузка данных...</span>
                    </p>
                </div>
            </div>
            <script type="text/javascript">
            <!--
                function WaitAnswer() {
                    var wait_answer = document.getElementById("wait_answer");
                    wait_answer.style.visibility = "visible";
                }
            -->
            </script>
            
            </body>
            </html>
        """

        let parseStartPage (page: string) =
            page
            |> Web.Parser.Html.load
            |> Result.mapError Infrastructure
            |> Result.bind (hasError)
            |> Result.bind (getNodes "//input | //img")
            |> Result.bind (fun nodes ->
                nodes
                |> Seq.map (fun node ->
                    match node.Name with
                    | "img" ->
                        let captchaCode = node |> getAttributeValue "src"

                        match captchaCode with
                        | Ok captchaCode when captchaCode.Contains("CodeImage") -> Ok <| Some("captcha", captchaCode)
                        | _ -> Error(Logical(Business "Required attribute was not recognized"))
                    | "input" ->
                        let name = node |> getAttributeValue "name"
                        let value = node |> getAttributeValue "value"

                        match name, value with
                        | Ok name, Ok value -> Ok <| Some(name, value)
                        | _ -> Error(Logical(Business "Required attribute was not recognized"))
                    | _ -> Ok None

                )
                |> Seq.roe
                |> Result.map (Seq.choose id >> Map.ofSeq))

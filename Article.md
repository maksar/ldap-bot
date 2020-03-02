# JSON Parsing

Что-то давно на Workplace не видно технических статей, спешу исправиться. В связи с задержкой, "градус гиковости" будет временно значительно повышен.

В публикации на прошлой неделе я [рассказал](https://itransition.workplace.com/groups/1394518670594144/permalink/2957160354329960/) про [новый бот](https://itransition.workplace.com/chat/t/105678357661487) для Workplace, который помогает управлять проектными (и не только) группами не прибегая к помощи `HelpDesk`.

Он написан на языке программирования `Haskell`, что для нашей компании выбор не совсем типичный (хотя с RFX-ами на эту тему к нам обращались). Знаниями надо делиться, так что запланировал несколько статей по мотивам написания этого бота. Статьи будут раскрывать некоторые интересные аспекты реализации, которые, на первый взгляд, могут показаться странными или неочевидными, но все же позволяют по новому взглянуть на типовые задачи, возникающие при разработке ПО.

## Почему `Haskell`?

Меня всегда привлекал этот язык программирования своей строгостью, лаконичностью и близостью к математике. Однако, написать на нем что-то более-менее крупное - шанса все не представлялось. Да, были небольшие pet-проекты, курсы по решению алгоритмических задач, но до "полноценного" production использования дело не доходило.

Но недавно я в очередной раз посетил конференцию по функциональному программированию - [F(by)](https://fby.dev) и твердо решил – в этот раз (пока мотивация от докладов не прошла) - надо обязательно это сделать! Задача возникла совершенно естественным способом, из рутины. В настоящее время, многих менеджеров (а в последствии и всех остальных сотрудников) перевели на использование облачных учетных записей `Microsoft Office`. У них пропала возможность самостоятельно редактировать состав проектных групп.

Дело в том, что синхронизация между наземным хранилищем и "облаком" может быть настроена только однонаправленная ("земля-воздух" кхе-хе). Так как `Outlook` у менеджеров уже облачный, то изменения, которые они пытаются с его помощью сделать, не могут попасть в наш `Active Directory`. Предлагаемый `MIDS` путь - создавать запросы в `HelpDesk` - меня категорически не устраивал. Вот и пришла идея этот процесс автоматизировать.

План статей пока выходит примерно такой:

* Parsing different JSON payloads into a single data structure
* Type-safe API for server endpoints and clients
* Parsing environment variables with reverse tests
* First-class Effects with pure testing
* Static linking and Dockerize application

## Parsing different JSON payloads into a single data structure

Чтобы не затягивать - начнем с первой темы ;)

Обычно, при разработке `API Endpoint`-ов принято иметь структуры данных, которые отражают принимаемый `json` один-к-одному. А только потом извлекать из него значения, полезные/нужные для работы программы. Так делается для... простоты. Программист знает формат `json`-а, который будет на входе его сервиса и либо (в случае динамического языка программирования) парсит этот `json` как нетипизированный `Value`, либо (в случае статической типизации) парсит его в экземпляр класса, отражающий структуру приходящего `json`-а.

Подход, сам по себе не плох, но появляется промежуточный слой `DTO`, для работы приложения совершенно не обязательный. Особенно если достоверно известно (как раз мой случай), что формат этого `json`-а вряд-ли изменится в скором времени – `json`-ы мне присылает `Facebook`.

Пропустить промежуточную `DTO` можно написав собственный парсер, который сразу преобразует `json` в нужную для работы системы структуру данных. Звучит довольно сложно, ведь все привыкли использовать для разбора `json`-а готовые библиотеки, основанные на аннотациях (в случае статических языков) либо парсить `json` "в нетипизированный объект".

К счастью в `Haskell` дела с парсингом (всего, не только `json`) исторически обстоят намного лучше. Существуют библиотеки так называемых `parsing combinator`-ов, для создания эффективных парсеров при помощи композиции (композиция функций – краеугольный камень функционального программирования). С их помощью можно распарсить `json` прямо в нужную вам структуру данных.

`Facebook` (`Workplace`) присылает боту примерно такой `json` в случае поступления сообщения от пользователя:

```json
  { "object": "page",
    "entry": [{"id": "entry_id", "time": 123,
      "messaging": [{
        "sender": {"id": "sender_id", "community": {"id": "community_id"}},
        "recipient": {"id": "recipient_id"}, "timestamp": 123,
        "message": {"mid": "mid", "text": "text"}}]}]}
```

И такой `json` в случае, если пользователь нажал на кнопку из `help` сообщения.

```json
  { "object": "page",
    "entry": [{"id": "entry_id", "time": 123,
      "messaging": [{"sender": {"id": "sender_id", "community": {"id": "community_id"}},
        "recipient": {"id": "recipient_id"}, "timestamp": 123,
        "postback": {"title": "postback_title", "payload": "payload"}}]}]}
```

Обратите внимание на последнюю строку `json` сообщения, в первом случае передается `message`, а во втором `postback`. Данных много, но мне из этого всего нужен только `sender_id` - уникальный идентификатор отправителя (нужен для того, чтобы послать ему ответ) и `text` либо `payload` - текст сообщения, которое пользователь послал боту, либо `payload` (свойство `payload` назначается кнопке на help сообщении и присылается боту при ее нажатии пользователем).

Парсить все это я буду в такую незатейливую структуру данных:

```haskell
data Messages = Messages
  { messages :: NonEmpty Message
  }

data Message = Message
  { sender_id :: String
  , text      :: String
  }
```

Для парсинга была выбрана стандартная для этой задачи библиотека `Aeson`, требующая "реализовать" интерфейс `FromJSON`. Не пугаемся незнакомому синтаксису, я все объясню...

```haskell
instance FromJSON Messages where
  parseJSON = withObject "root object" $ \root ->
    root .: "entry" >>= fmap (Messages . fromList . toList  . join) . withArray "entries array"
      (mapM $ withObject "entry object" $ \entry ->
        entry .: "messaging" >>= withArray "messaging array"
          (mapM $ withObject "message object" $ \message ->
            Message
              <$> (message .: "sender" >>= (.: "id"))
              <*> (  (message .: "message" >>= (.: "text"))
                 <|> (message .:  "postback" >>= (.: "payload"))
                  )
          )
      )
```

Основой является функция `withObject`, первый параметр которой служебный - название объекта, который мы собираемся парсить. Первый, самый главный объект обзовём `root object`. Второй параметр - это λ (lambda) - то есть функция, которая на вход принимает уже распаршенный `root` объект и дальше вольна делать с ним все что ей хочется. А хочется ей взять (при помощи оператора `.:`, чтобы было похоже на разделитель `:` ключ-значение из `json`-а) из `root` объекта поле по ключу `"entry"` и начать его парсить (`>>=`) дальше.

Пока опустим магию `fmap (Messages . fromList . toList  . join)`, о ней позже. Что в `json`-е лежит по ключу `"entry"`? А там массив, значит необходимо воспользоваться функцией `withArray` первый параметр которой, по традиции - описание того, что сейчас парсим. Нужны эти описания, к слову, для того, чтобы при ошибке парсинга вывести понятную ошибку, например ошибка для `json`-а `{"object": "page", "entry": 123}` будет такая: `parsing entries array failed, expected Array, but encountered Number`. Так что наличие этих описаний полезно как для `debug`-а, так и для информативности ошибок будущего софта.

Парсим `entry object`, `messaging array` и `message object` уже знакомыми нам `withObject` и `withArray`, попутно не забывая итерироваться по ним при помощи `mapM` (аналог простого `map`, парсим мы все же массивы, на выходе тоже должны быть массивы). Подошли к самому интересному, созданию итоговых объектов `Message`.

Конструктор `Message` (в данном случае `Message` – это название "конструктора" для создания одноименной структуры `Message`), принимает две строки - `sender_id` и `text`. В Процессе парсинга, у нас нет "строк" (с типом `String`), есть только "парсеры, которые могут вернуть строку" (с типом `Parser String`). Так что приходится пользоваться операторами `<$>` и `<*>` для того, чтобы увязать парсеры строк и строки между собой. Фактически, оператором `<$>` мы "учим" конструктор `Message` принимать вместо строк - парсеры строк.

На месте первого параметра (там где должен быть `sender_id`) передаем парсер `message .: "sender" >>= (.: "id")` - его можно перевести на "человеческий" язык как "когда я буду парсить `message`, я возьму у него свойство `sender`, а у его содержимого возьму свойство `id`". То есть этот парсер, способен обработать `json` `"sender": {"id": "sender_id", "community": {"id": "community_id"}}`, вернув при этом только `sender_id` и проигнорировав все остальное, чего нам и нужно.

Аналогичным образом можно поступить и с `text` только вот не всегда `"message": {"mid": "mid", "text": "text"}}` от `Facebook` в этом месте приходит, иногда ещё и `"postback": {"title": "postback_title", "payload": "payload"}}` может быть. Мощь и изящество `parsing combinator`-ов раскрывается как раз в таких случаях. Комбинатор `<|>` говорит - сначала попытайся применить парсер, который слева от меня `(message .: "message" >>= (.: "text")`, а если он вернёт ошибку парсинга - попробуй тот, который от меня справа `message .:  "postback" >>= (.: "payload")`. В итоге, выражение `(message .: "message" >>= (.: "text")) <|> (message .:  "postback" >>= (.: "payload"))` распарсит либо цепочку `message->text` либо `postpack->payload` и вернет строку `String`. Мы скомбинировали два строковых парсера и получили на выходе тоже "парсер строк", реализующий собой операцию "выбора", на что намекал знак `|` в комбинаторе `<|>`.

Вспомним теперь про два вложенных друг в друга `mapM`. На уровне `root object`-а получается, что мы сформировали список списков сообщений, точнее вектор векторов (так как `Aeson` работает с векторами а не списками) то есть `Vector (Vector Message)`. Для его "схлопывания" применим `join`, превратив `Vector (Vector Message)` в `Vector Message`, затем (операцию `.` стоит "читать" слева направо, так как он право-ассоциативен) конвертируем `Vector` в список при помощи `toList`, список в `NonEmpty` (это вид списков, которые не могут быть пусты, ведь должно же в нотификации от `Facebook` быть хотя бы одно сообщение пользователя) при помощи `fromList` и передадим это все в конструктор `Messages`.

Ух, похоже это тот самый случай, когда объяснение кода заняло раз в 10 больше символов, чем сам код... Но что в итоге? Мы можем парсить два разных сообщения в одну структуру данных, с которой работает бот. Для него ведь не важно, сам пользователь написал в чате `/help` или воспользовался кнопкой-подсказкой. Реагировать бот на это должен одинаково. Тесты успешно проходят:

```haskell
describe "Messages spec" $ do
  let decoding :: Text -> Messages
      decoding = fromJust . decode . pack . unpack

  it "parses text message properly" $ do
    decoding [I.text|
      { "object": "page",
        "entry": [{"id": "id", "time": 1,
          "messaging": [{
            "sender": {"id": "sender_id", "community": {"id": "id"}},
            "recipient": {"id": "id"}, "timestamp": 1,
            "message": {"mid": "mid", "text": "text"}}]}]}
    |] `shouldBe` (Messages $ (Message "sender_id" "text") :| [])

  it "parses postback message properly" $ do
    decoding [I.text|
      { "object": "page",
        "entry": [{"id": "id", "time": 1,
          "messaging": [{"sender": {"id": "sender_id", "community": {"id": "id"}},
            "recipient": {"id": "id"}, "timestamp": 1,
            "postback": {"title": "title", "payload": "payload"}}]}]}
    |] `shouldBe` (Messages $ Message "sender_id" "payload" :| [])

  it "fails to parse incomplete json" $ do
    (eitherDecode "{\"object\": \"page\"}" :: Either String Messages) `shouldBe` Left "Error in $: key \"entry\" not found"
```

# Servant

Итак - подошло время очередной технической статьи. На этот раз речь пойдет про API-over-HTTP. Вроде банальнейшая вещь, каждый так "сто раз делал" и чего вообще можно было на эту тему необычного придумать... Действительно, практически в любом backend-е есть слой "контроллеров", который отвечает за то, чтобы функции приложения были доступны извне по протоколу `http`. Кто-то использует `json`, кто-то `xml`, но общий знаменатель всегда - `http`.

`REST API` - давно стал стандартом де-факто. Все привыкли к модели ресурсов-существительных и стандартных глаголов-действий `CRUDL`. В code review я сам часто советую заменить action-ы `up` и `down` ресурса `vote` на два отдельных контроллера `upvote` и `downvote` с методом `create`, для соответствия принципам `REST`.

Но не `REST`-ом единым, как говорится, есть еще `GraphQL`, и много чего другого. Для очень маленьких приложений с одним-двумя endpoint-ами следовать заветам `REST` не так уж и необходимо. Сегодня мы как раз поговорим о подходе к API, который исповедует библиотека `Servant` из мира языка программирования `Haskell`, которую я использовал при написании бота [Group Manager](https://itransition.workplace.com/chat/t/105678357661487).

## API как тип

Библиотека `Servant` требует описать все ваше API в виде типа. Одного, весьма развесистого и длинного, но все-же типа (как `String` или `List Integer`). Рассмотрим пример из практики. Endpoint, реагирующий на оповещения от Facebook-а можно описать как:

```haskell
type MessageAPI = ReqBody '[JSON] Messages :> Post '[JSON] (NonEmpty SendTextMessageResponse)
```

Этот тип, состоит из двух частей, разделенных комбинатором `:>` (читать стоит как... как стрелку, например). Даже не зная всей специфики синтаксиса, можно догадаться, что речь идет о `POST` запросе, который в body принимает сообщение типа `Messages` в виде `json`-а и возвращает непустой список `SendTextMessageResponse`-ов, так же в виде `json`-а.

API редко состоит только из одного endpoint-а. Наш случай - не исключение, Facebook требует, чтобы у принимающей нотификации стороны был еще один метод, для [верификации endpoint-а](https://developers.facebook.com/docs/graph-api/webhooks/getting-started/#verification-requests), добавим его.

```haskell
type WebHookAPI =
       ReqBody '[JSON] Messages :> Post '[JSON] (NonEmpty SendTextMessageResponse)
  :<|> RequiredParam "hub.verify_token" Text :> RequiredParam "hub.challenge" Text :> Get '[PlainText] Text
```

При помощи комбинатора `:<|>` (это не emoji, это аналог операции "альтернатива" `<|>` из [предыдущей статьи](https://itransition.workplace.com/groups/143641062970056/permalink/484712628862896/)) к первому запросу добавился еще один: он реагирует на `GET` запрос, требует наличия двух текстовых параметров и отвечает plain текстом, без всякого `json`-а. Лишь только посмотрев на тип можно сразу понять протокол взаимодействия приложения с окружающим миром, не обращаясь к документации, не рыща по исходникам в поисках аннотаций над контроллерами и их методами.

Так же как и части типа объединяются между собой комбинатором `:<|>`, так и реализации этих endpoint-ов можно объединить в одно целое.

```haskell
webhookMessage :: Messages -> Handler (NonEmpty SendTextMessageResponse)
webhookMessage = ... -- implementation omitted

webhookVerify :: Text -> Text -> Handler Text
webhookVerify = ... -- implementation omitted

entireAPI = webhookVerify :<|> webhookMessage
```

При этом их типы тоже объединятся. Не будем утруждать себя и спросим у `REPL`-а (в комплируемых языках `REPL` – не редкость):

```haskell
> :t entireAPI
< entireAPI :: Messages -> Handler (NonEmpty SendTextMessageResponse) :<|> Text -> Text -> Handler Text
```

## Возможности

Компилятор не даст собрать систему, в которой программист "забыл" обработать какой-то параметр или пытается ответить текстом на запрос, в контракте ответа которого требуется список. Библиотека `Servant` берет на себя много рутинной работы по ответу на запросы, которые "не обрабатываются", то есть не описаны (не предусмотрены) в типе. Так же `Servant` занимается операциями `encode`/`decode` данных в/из `json` или `xml` форматы в соответствии с заявленным в типе и обработкой ошибок, связанных с этим.

Но все же, пока ничего экстраординарного, ну описан контракт в виде типа, что с того... Наверное дело в том, что можно удобно будет описывать повторяющиеся части API? Написали один раз параметризованный тип:

```haskell
--   - GET /<name>
--   - GET /<name>/id
--   - POST /<name>
type CreateReadList (name :: Symbol) a = name :>
 (                            Get  '[JSON] [a]
 :<|> Capture "id" Integer :> Get  '[JSON] a
 :<|> ReqBody '[JSON] a    :> Post '[JSON] NoContent
 )
```

И используем его для нескольких типов сущностей:

```haskell
type API = FactoringAPI
     :<|> CreateReadList "users" User
     :<|> CreateReadList "products" Product
```

Если захотим в ответ на `POST` запрос для создания сущности начать что-то возвращать (например id созданной записи), то изменение сделанное в одном месте (вместо `NoContent` напишем `Integer`) отразится сразу и на `user`-ах и на `product`-ах, причем компилятор нам точно скажет где именно в коде начало возникать несовпадение типов, чтобы мы точно не забыли вернуть `Integer` из обработчика запроса.

Но эта кроличья нора несколько глубже... Так как тип известен на этапе компиляции, а в Haskell есть интроспекция типов (тоже на этапе компиляции), то можно информацию из типа использовать для... генерации кода!

Объявляемый тип API представляет собой контракт обмена сообщениями. Но сообщения же можно не только принимать, но еще и отправлять! Бот [Group Manager](https://itransition.workplace.com/chat/t/105678357661487) тоже вынужден это делать для общения с пользователем. Facebook не обращает внимания на то, что вы ему шлете в ответ на нотификацию о сообщении от пользователя, ему главное чтобы HTTP код был 200. Для того, чтобы пользователю написать – нужно воспользоваться специальным [Facebook Messaging API](https://developers.facebook.com/docs/messenger-platform/reference/send-api/), то есть послать несколько сообщений Facebook-у по HTTP. А что если описать и этот протокол взаимодействия в виде типа?

```haskell
type RequiredParam = QueryParam' '[Strict, Required]
type AccessTokenParam = RequiredParam "access_token" Text

type FBMessengerSendAPI =
       "me" :> "messages" :> ReqBody '[JSON] SendTextMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
  :<|> "me" :> "messages" :> ReqBody '[JSON] ServiceMessageRequest :> AccessTokenParam :> Post '[JSON] ()
  :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> AccessTokenParam :> Get '[JSON] UserInfo
```

Первый и второй API вызовы выглядят похожими. С точки зрения Facebook это, вообще говоря, один и тот же `GET` endpoint на URL-е `"/me/messages"`, который принимает `json` в body, но с точки зрения нас, как потребителя этой API, вызовы разные, с разным назначением и даже возвращаемым типом (в случае служебных сообщений нам "не важно" что Facebook на него ответил).

Прелесть в том, что код для методов доступа к такому API может быть автоматически сгенерирован, нужно только немного помочь компилятору, написав "заглушки" методов с сигнатурами типов:

```haskell
sendTextMessage :: SendTextMessageRequest -> Token -> ClientM SendTextMessageResponse
sendServiceMessage :: ServiceMessageRequest -> Token -> ClientM SendTextMessageResponse
getUserInfo :: Text -> Text -> Token -> ClientM UserInfo

sendTextMessage :<|> sendServiceMessage :<|> getUserInfo = client (Proxy :: Proxy FBMessengerSendAPI)
```

Пользоваться методами можно предоставив "направление" `BaseUrl Https "graph.facebook.com" 443 "/v6.0"`:

```haskell
runClientM (getUserInfo "123" "email" (Token "access_token")) $ with graphAPIBaseUrl >>= \case
  Left error     -> -- Do something with error
  Right userInfo -> -- userInfo from Facebook, has type UserInfo
```

Пропадает необходимость работы с низкоуровневыми HTTP библиотеками, нет нужды вручную заниматься чтением `json`-а из ответа сервера, даже строить URL-ы самому не надо (обратите внимание, метод `getUserInfo` ничего об URL-е "не знает").

Подход, среди прочего, позволяет:
- на основании информации из типа сгенерировать код на [JavaScript](https://hackage.haskell.org/package/servant-js) (или на другом [языке](https://hackage.haskell.org/package/servant-foreign)) для доступа к такой API-шке;
- создать [Swagger](https://hackage.haskell.org/package/servant-swagger-1.1.8/docs/Servant-Swagger.html) описание API-шки из ее типа либо наоборот, сгенерировать тип на основе Swagger описания;
- в несколько строк создать [mock](https://hackage.haskell.org/package/servant-mock) версию API-шки, которая бы возвращала случайные данные, но в строгом в соответствии с ожидаемым форматом;
- сгенерировать [документацию](https://hackage.haskell.org/package/servant-docs) в markdown формате с описанием и примерами использования;
- написать [тест](https://hackage.haskell.org/package/servant-quickcheck-0.0.8.0/docs/Servant-QuickCheck.html), который будет "долбить" все наши endpoint-ы запросами со случайными данными проверяя предикаты `not500 <%> notLongerThan 1000000` (для целей нагрузочного тестирования) или `onlyJsonObjects` (чтобы отловить "ошибки дизайна" API вида `Post '[JSON] ()`).

И это не теоретические "возможности", для всего есть рабочие библиотеки. Более того, начали появляться [реализации](https://www.47deg.com/blog/introducing-mu-haskell-0-1/) той же идеи, но вместо `HTTP REST` использующие `gRPC` (говорят сейчас так модно в мире микро-сервисных архитектур).

## Refactoring

"Сломать" работающий сервис в процессе рефакторинга становится крайне проблематично. К примеру, решили мы избавиться от дублирования в описании типа `FBMessengerSendAPI`. В нем несколько раз повторяется часть, моделирующая префикс URL-а "/me/messages", да и описывать в каждом из endpoint-ов факт того, что "надо бы token передать" утомительно.

Прямо как в алгебраическом уравнении, "выносим за скобки" `AccessTokenParam`, а затем и префикс `"me" :> "messages"`. В результате token будет применяться ко всем endpoint-ам, а префикс, только к первым двум (в соответствии со свойством [дистрибутивности](https://ru.wikipedia.org/wiki/%D0%94%D0%B8%D1%81%D1%82%D1%80%D0%B8%D0%B1%D1%83%D1%82%D0%B8%D0%B2%D0%BD%D0%BE%D1%81%D1%82%D1%8C)).

```haskell
-- Initial version
type FBMessengerSendAPI =
       "me" :> "messages" :> ReqBody '[JSON] SendTextMessageRequest :> AccessTokenParam :> Post '[JSON] SendTextMessageResponse
  :<|> "me" :> "messages" :> ReqBody '[JSON] ServiceMessageRequest :> AccessTokenParam :> Post '[JSON] ()
  :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> AccessTokenParam :> Get '[JSON] UserInfo

-- Step 1 - extracting AccessTokenParam
type FBMessengerSendAPI =
 AccessTokenParam :> (
       "me" :> "messages" :> ReqBody '[JSON] SendTextMessageRequest :> Post '[JSON] SendTextMessageResponse
  :<|> "me" :> "messages" :> ReqBody '[JSON] ServiceMessageRequest  :> Post '[JSON] SendTextMessageResponse
  :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> Get '[JSON] GetUserInfoMessageResponse)

-- Step 2 - extracting "me" :> "messages"
type FBMessengerSendAPI =
 AccessTokenParam :> (
    "me" :> "messages" :> (
         ReqBody '[JSON] SendTextMessageRequest :> Post '[JSON] SendTextMessageResponse
    :<|> ReqBody '[JSON] ServiceMessageRequest  :> Post '[JSON] SendTextMessageResponse)
  :<|> Capture "user_id" Text :> RequiredParam "fields" Text :> Get '[JSON] GetUserInfoMessageResponse)
```

Соответственно, сигнатуры методов доступа к данным, тоже должны измениться. Раньше token был последним параметром, а станет первым:

```haskell
sendTextMessage :: Token -> SendTextMessageRequest -> ClientM SendTextMessageResponse
sendServiceMessage :: Token -> ServiceMessageRequest -> ClientM SendTextMessageResponse
getUserInfo :: Token -> Text -> Text -> ClientM UserInfo

sendTextMessage :<|> sendServiceMessage :<|> getUserInfo = client (Proxy :: Proxy (Flat FBMessengerSendAPI))
```

А так как они изменились, то компилятор будет ругаться на все их использования в коде, не позволяя нам нечаянно "забыть" поменять порядок в одном из мест. Строгая и мощная система типов не всегда "стоит на пути", чаще она защищает от ошибок и предотвращает потенциальные баги ;)

# ENV parsing

Тема сегодняшней статьи – чтение конфигурационных значений из переменных окружения и связанные с этим процессом трудности. В небольших системах, где нет необходимости в полноценном конфигурационном файле, принято брать настройки из переменных окружения, это один из ключевых моментов [12 factor app](https://12factor.net/config) манифеста. Это надежный и относительно безопасный способ конфигурации, он отлично поддерживается всеми операционными системами, облачными платформами и средствами контейнеризации.

"Так а что сложного-то?" спросите вы, "в любом языке программирования есть для этого встроенные средства, сдобренные ни одним десятком библиотек, упрощающих этот процесс. Действительно, проблем с тем, чтобы прочитать значение переменной окружения нет. Но если подходить к задаче не системно, запрашивая значения переменных окружения там и тут в коде, трудности все-же начнутся. Такую программу будет сложно сопровождать, так как существует множество мест в коде системы, где идет обращение к одной и той же переменной окружения. Но самое важное – такую систему будет сложно тестировать – необходимо использовать дополнительные ухищрения для подмены значений переменных окружения в тестовом режиме работы. Трудностей, со временем, будет становиться все больше, так как с добавлением нового функционала вырастет и количество настроек.

Способ преодоления таких трудностей эволюционно-естественен – необходимо сконцентрировать работу с конфигурацией в одном месте, сделать процесс добавления новой настройки понятным, упростить доступ к настройкам в коде бизнес-логики.

## Постановка задачи

В `Haskell`, несмотря на всю его "строгость" и приверженность к математически чистым функциям, тоже можно обращаться к переменным окружения откуда угодно, но "тут так не принято"... Язык подталкивает программиста отказаться от идеи так делать, заставляя явно отказываться от "чистоты" функций и терять все связанные с этим свойством преимущества. В мире строго-типизированных языков "удобно" не читать "настройки" посреди кода с логикой, а читать их в начале исполнения программы, преобразовать во внутреннюю структуру данных (с адекватными типами вместо строк) и использовать явно передавая такую структуру или ее части в остальные "вычисления" оставляя их свободными от side-effect-ов.

Довольно философствований, show me the code, как говорится.

```haskell
data Config = Config
  { _ldapHost               :: Text
  , _ldapPort               :: PortNumber
  , _port                   :: Int
  , _verifyToken            :: Text
  , _pageToken              :: Text
  , _user                   :: Text
  , _password               :: Text
  , _activeUsersContainer   :: Dn
  , _projectGroupsContainer :: Dn
  , _projectGroupsOrgunits  :: NonEmpty Text
  }
  deriving (Eq, Show, Generic, Default)
```

`Config` – та самая структура данных с настройками, необходимыми для работы [Group Manager](https://itransition.workplace.com/chat/t/105678357661487) бота. В начале работы системы, эта структура заполняется значениями из переменных окружения.

```haskell
readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig =
  Config <$> lookupText "LDABOT_LDAP_HOST"
         <*> lookupNumber "LDABOT_LDAP_PORT"
         <*> lookupNumber "LDABOT_PORT"
         <*> lookupText "LDABOT_VERIFY_TOKEN"
         <*> lookupText "LDABOT_PAGE_TOKEN"
         <*> lookupText "LDABOT_USERNAME"
         <*> lookupText "LDABOT_PASSWORD"
         <*> (Dn <$> lookupText "LDABOT_USERS_CONTAINER")
         <*> (Dn <$> lookupText "LDABOT_GROUPS_CONTAINER")
         <*> (fromList . splitOn "," <$> lookupText "LDABOT_GROUPS_ORGUNITS")
```

Как ни странно, функция `readConfig` является "чистой", хотя вроде бы и обращается к внешнему миру (то есть имеет side-effect-ы). Почему это так и как работает – я расскажу в следующей статье про "алгебраические эффекты". А пока, еще немного деталей реализации:

```haskell
lookupText :: (Member Environment r, Member (Error Text) r) => Text -> Sem r Text
lookupText name = lookupEnv name >>= \case
    Nothing     -> throw $ unwords ["Please set", name, "environment variable."]
    Just string -> return $ pack string

lookupNumber :: (Read a, Member Environment r, Member (Error Text) r) => Text -> Sem r a
lookupNumber name = read . unpack <$> lookupText name
```

Функция `lookupText` обращается к операционной системе через `lookupEnv name` и анализирует результат. Если значения не оказалось – генерируется ошибка, в противном случае – функция возвращает значение переменной окружения. `lookupNumber` является надстройкой над `lookupText`, которая после успешного получения значения конвертирует его в число. Интересным моментом тут является оператор `<$>` (так же известный как `fmap` в `Haskell` или `Optional.map` в `Java`). Его использование позволяет "не засорять" код обработкой граничных случаев вида "если `lookupText` вернул `null`, то тоже вернуть `null`; в противном случае – преобразовать в число и вернуть". Если вы вспомнили про elvis-оператор, то знайте, он является лишь частным случаем `fmap` для `null`-ов ;)

`<$>` несколько раз применяется еще и внутри `readConfig` для тех же целей – преобразовывать прочитанное из `LDABOT_USERS_CONTAINER` в `Dn` (термин из мира `LDAP`, означает `distinguished name`) есть смысл только если там что-то было. Самое первое использование `<$>` немного интереснее. Помните рассказ про `<$>` из [первой статьи](https://itransition.workplace.com/groups/143641062970056/permalink/484712628862896/) про парсинг `json`-а? Речь шла о том, чтобы "адаптировать" конструктор структуры данных `Message` (который принимает строки) к "парсеру строк". Если посмотреть на такую адаптацию с другой стороны – операция `<$>` превращала "парсер строк" в "парсер `Message`-ей" постулируя "когда (и если) оригинальный парсер строк что-нибудь вернет, примени к этому конструктор `Message`".

С `Config`-ом ситуация та же, оператор `<$>` постулирует "когда (и если) **все** операнды для вызова функции `Config` будут готовы – вызывай". Если ранее мы конструировали `Message` "в контексте" парсера, который может ничего "не напарсить", то сейчас мы конструируем `Config` "в контексте" вычисления, которое может вернуть ошибку. `fmap` – он как обычный `map`, только не для списков, а для любых "контейнеров" или "вычислений" (деревья, Optional, парсер, генератор). Подготовка операндов происходит при помощи `<*>`. Его отличие от `<$>` в том, что теперь с обоих сторон "вычисления, которые могут вернуть ошибку". Механика сложная, зато код элегантный, без постоянных проверок (привет программистам на `golang`) и early return-ов.

## Тестирование

С проблематикой вроде разобрались, пора начинать извлекать пользу. из "централизации" работы с настройками а так же от использования "чистых" функций (не зря же прилагались усилия). С точки зрения кода, читающего значения переменных – совершенно не важно откуда именно происходит чтение – из реальных переменных окружения или из заранее подготовленного ассоциативного массива, главное, чтобы `lookupEnv` возвращала `Maybe Text`. Определив "тестовое окружение" как простой писок ключ-значение `type EnvironmentMock = [(Text, Text)]`, можно заставить `readConfig` читать данные из заранее подготовленного места.

```haskell
withMockedEnvironment :: EnvironmentMock -> Sem '[Environment, Error Text] a -> Either Text a
withMockedEnvironment mockedEnv = run . runError . fakeEnvironment mockedEnv

fakeEnvironment :: Member (Error Text) r => EnvironmentMock -> InterpreterFor Environment r
fakeEnvironment mockedEnv = interpret $ \case
  LookupEnv name -> return $ unpack <$> lookup name mockedEnv

withMockedEnvironment
  [ ("LDABOT_LDAP_HOST", "host")
  , ("LDABOT_LDAP_PORT", "123")
  , ("LDABOT_PORT", "234")
  , ("LDABOT_VERIFY_TOKEN", "vtoken")
  , ("LDABOT_PAGE_TOKEN", "ptoken")
  , ("LDABOT_USERNAME", "user")
  , ("LDABOT_PASSWORD", "pass")
  , ("LDABOT_USERS_CONTAINER", "ucont")
  , ("LDABOT_GROUPS_CONTAINER", "gcont")
  , ("LDABOT_GROUPS_ORGUNITS", "ou1,ou2")
  ] readConfig `shouldBe` Right Config {
    _ldapHost = "host",
    _ldapPort = 123,
    _port = 234,
    _verifyToken = "vtoken",
    _pageToken = "ptoken",
    _user = "user",
    _password = "pass",
    _activeUsersContainer = Dn "ucont",
    _projectGroupsContainer = Dn "gcont",
    _projectGroupsOrgunits = "ou1" :| ["ou2"]
  }
```

Как говорит один мой знакомый, "мало вариативности". Он ярый поклонник разработки через тесты, (привет тебе, С.В.)". Ну что-ж, постараемся добавить вариативности и уважить скептиков, заявляющих при чтении таких тестов – "а как убедиться в том, что реализация не состоит из хардкода именно этих значений".

Есть такой прием в тестировании – проверять обратимость (`reverse(reverse(list)) === list`). Построение конфига из окружения - назовем прямым преобразованием Окружение -> Конфиг. Если бы у нас было обратное преобразование (из Конфига в Окружение, из которого такой Конфиг прочитан), то мы бы могли проверить, что применив сначала прямое преобрзование, а затем обратное – получается исходный Конфиг. Такую пару Окружения и Конфига называют изоморфной, а само преобразование – изоморфизмом. Как обычно бывает в математике – слово сложное, но за ним стоит простая идея ;)

Если сначала конфиг (абсолютно любой) преобразовать в набор пар ключ-значение, а потом из них попытаться "прочитать" конфиг обратно, то в итоге должны ведь получить исходный конфиг.

```haskell
toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock Config {_ldapHost, _ldapPort, _port, _verifyToken, _pageToken, _user, _password, _activeUsersContainer, _projectGroupsContainer, _projectGroupsOrgunits} =
  [ ("LDABOT_LDAP_HOST", unpack _ldapHost)
  , ("LDABOT_LDAP_PORT", show _ldapPort)
  , ("LDABOT_PORT", show _port)
  , ("LDABOT_VERIFY_TOKEN", unpack _verifyToken)
  , ("LDABOT_PAGE_TOKEN", unpack _pageToken)
  , ("LDABOT_USERNAME", unpack _user)
  , ("LDABOT_PASSWORD", unpack _password)
  , ("LDABOT_USERS_CONTAINER", fromDn _activeUsersContainer)
  , ("LDABOT_GROUPS_CONTAINER", fromDn _projectGroupsContainer)
  , ("LDABOT_GROUPS_ORGUNITS", unpack $ intercalate "," $ toList _projectGroupsOrgunits)]
  where
    fromDn (Dn dn) = unpack dn
```

Имея прямое и обратное преобразование, можно записать:

```haskell
it "reads config from complete environment" $ forAll $ \config ->
  withMockedEnvironment (toEnvironmentMock config) readConfig === Right config
```

Но это только success случай мы протестировали, пока не ясно как будет себя вести функция чтения конфига, если в переменных окружения будет отсутствовать одно из значений. Но погодите-ка – ведь у нас же есть способ получить окружение в виде списка ключ-значение. Достаточно только удалить из нее одну (случайную) строку и попытаться прочитать конфиг:

```haskell
it "fails to read a config from incomplete environment" $ forAll $ \config -> do
  shuffled <- shuffle $ toEnvironmentMock config
  let ((missingKey, _), incompleteMock) = (head shuffled, tail shuffled)
  return $ withMockedEnvironment incompleteMock readConfig === Left (unwords ["Please set", missingKey, "environment variable."])
```

Ну вот, кажется удалось свести задачу тестирования функции чтения конфигурации к формированию произвольных конфигов. Эта задача для `Haskell` довольно типична – использовать property-based тестирование на нем очень любят. Так как структура `Config` состоит из достаточно примитивных типов и оберток над ними, то "произвольность" можно обеспечить с помощью всего нескольких строк.

```haskell
makeArbitrary ''Config

instance Arbitrary Config where
  arbitrary = arbitraryConfig
  shrink = recursivelyShrink
```

Благодаря тому, что `Config` теперь "реализует" `Arbitrary`, можно создавать "генератор" конфигов – `Gen Config` при помощи функции `arbitrary` из класса.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Попробуем в REPL-е сгенерировать что-нибудь случайное:

```haskell
< sample (arbitrary :: Gen Config)

> Config {_ldapHost = "\nIUZ\DELu\EMUG1\DEL\1002298\11790\DC3s\STX", _ldapPort = 20, _port = -17, _verifyToken = "\SO\DLE9_1\NUL\210889\681130l\ENQ", _pageToken = "q\r;h1\959827\&1~\703396P1~\837562\190001xjf", _user = "\466790\&6\DC4j", _password = "{H", _activeUsersContainer = Dn "", _projectGroupsContainer = Dn "U\ACK\616135\570186v\672268\571313", _projectGroupsOrgunits = "\615852L$\598568\ESC6\fc" :| ["[h\DC4N[3pzk\b\SUB6\133277\14775"]}
```

Работает! Теперь полиморфная функция `forAll`, обладающая типом `forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property` может принимать на вход "генератор конфигов" и проверять `Property` (по сути, чуть-чуть более хитрый предикат, где вместо `==` используется `===`).

```sh
Env
  environment reading
    reads config from complete environment
      +++ OK, passed 100 tests.
    fails to read a config from incomplete environment
      +++ OK, passed 100 tests.
```

От каких "ошибок" защищают такие defensive (regression, golden) тесты? Например, если случайно переставить местами строки при построении конфига – тесты это отловят. Либо если попытаться захардкодить какое-нибудь одно значение на этапе построения конфига – тесты тоже просигнализируют с несовпадении значений (сгенерированное случайное значение будет отличаться от статического хардкода). Изменение названия переменных, из которых читаем конфиг, такой тест тоже "отловит", но отловит тут в кавычках, потому что такое падение теста не говорит о некорректности или неработоспособности программы, оно говорит лишь о том, что тесты нужно обновить, по сути "зашив" в процедуру генерации фейкового оружения новые названия переменных. В.С. непременно бы заметил еще на этапе написания тестов, что названия переменных повторяются и в реализации и в тестах – "не DRY", сказал бы он в code review комментарии...

## Суши с лупой

Для того, чтобы избавиться из повторений, будем использовать популярную в функциональном программировании вещь – линзы. Линза, если совсем просто ее представить, это такая сущность, которая совмещает в себе getter и setter. Ну как setter... программирование же функциональное, immutability везде, нет никаких setter-ов, есть только функции `Value -> Object -> Object`, которые не меняют `Object`, а возвращают новый.

В структуре данных `Config` не случайно свойства начинались с символа подчеркивания, этому есть причина: для каждого поля структуры, `Haskell` объявит одноименную функцию с сигнатурой, например `_ldapHost :: Config -> Text`. Если бы поле называлось `ldapHost`, то часто бы возникал конфликт имен при объявлении временных "переменных". Да и смотря на использование `ldapHost` в коде подсознательно думаешь о нем, как о значении, а не как о функции.

Эту конвенцию "эксплуатирует" библиотека `lens`, позволяющая одной строкой сгенерировать линзы для каждого из полей структуры.

```haskell
makeLenses ''Config

ldapHost :: Lens' Config Text
ldapPort :: Lens' Config PortNumber
...
```

Для чего вообще эти линзы удобны? Для работы со вложенным структурами данных в функциональном стиле. Имея список составных объектов.

```haskell
data Color = Color {_shade :: Text}
data Material = Material {_kind :: Text, _color :: Color}
data Player = Player {_name :: Text, _material :: Material}

makeLenses ''Color
makeLenses ''Material
makeLenses ''Player

let players = [Player "Bender" (Material "metal" (Color "shiny"))
              ,Player "Fry" (Material "meet" (Color "yellow"))
              ,Player "Leela" (Material "meet" (Color "purple"))]
```

Можно выполнять нетривиальные операции "вглубь" на immutable данных используя "композицию линз" через знакомый оператор `.`:

```haskell
< view material.color.shade $ head players
> "shiny"

< map (view $ material.color.shade) players
> ["shiny","yellow","purple"]

< map (over (material.color.shade) (append "super_")) players
> [Player {_name = "Bender", _material = Material {_kind = "metal", _color = Color {_shade = "super_shiny"}}}
  ,Player {_name = "Fry", _material = Material {_kind = "meet", _color = Color {_shade = "super_yellow"}}}
  ,Player {_name = "Leela", _material = Material {_kind = "meet", _color = Color {_shade = "super_purple"}}}]
```

Последний пример особенно нагляден, если бы не линзы, пришлось бы писать что-то вроде:

```haskell
map (\player ->
    let material = _material player
        color = _color material
        shade = _shade color
    in player { _material = material { _color = color { _shade = append "super_" shade } } }
  ) players
```

Вернемся к нашей задачу из избавлению от дублирования. Объявим список пар ключ-линза – никто не запрещает так сделать, ведь линза, по сути, всего-лишь сложная функция, а функции в `Haskell` first-class значения:

```haskell
settings = [
  ("LDABOT_LDAP_HOST",        ldapHost),
  ("LDABOT_LDAP_PORT",        ldapPort . isoRead . packed),
  ("LDABOT_PORT",             port . isoRead . packed),
  ("LDABOT_VERIFY_TOKEN",     verifyToken),
  ("LDABOT_PAGE_TOKEN",       pageToken),
  ("LDABOT_USERNAME",         user),
  ("LDABOT_PASSWORD",         password),
  ("LDABOT_USERS_CONTAINER",  activeUsersContainer . isoDn),
  ("LDABOT_GROUPS_CONTAINER", projectGroupsContainer . isoDn),
  ("LDABOT_GROUPS_ORGUNITS",  projectGroupsOrgunits . isoNonEmpty . splitted)]
  where
    isoRead :: (Read a, Show a) => Iso' a String
    isoRead     = iso show read
    isoDn       = iso (\(Dn dn) -> dn) Dn
    isoNonEmpty = iso toList fromList
    splitted    = iso (intercalate ",") (splitOn ",")
```

Обратите внимание на уже знакомые нам изоморфизмы снизу – пары функций, которые необходимы для преобразования линз к одному виду `Lens' Config Text`. Ведь исходя из типа `Config` линза `activeUsersContainer` работает с типом `Dn`, а мы хотим унифицировать все лизны в `settings` приведя их к одной, строковой сигнатуре.

Процедуру "чтения конфигурации" поменяем на свертку

```haskell
readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig = foldM reducer (Config {}) settings
  where
    reducer config (name, lens) = do
      value <- lookup name
      return $ set lens value config
```

Код осуществляет свертку `foldM` при помощи функции `reducer` списка `settings`, используя в качестве начального значения пустой `Config {}`. Функция `reducer` имеет на входе два параметра – `config` в качестве аккумулятор-а и пара ключ-линза из списка `settings`. Она читает (`lookup name`) значение переменной окружения, устанавливает прочитанное значение при помощи линзы в `config` и возвращает его. Таким образом, последовательно пройдясь по всему списку `settings` все поля структуры `Config` окажутся заполнены значениями.

Наконец-то мы можем избавиться от дублирования названий переменных в тестах. Вместо свертки, делаем простой обход списка `map` просматривая через линзу значения в `config`-е.

```haskell
toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock config = map (\(name, lens) -> (name, view lens config)) settings
```

Использование инверсии, идемпотентности и других инвариантов - здорово помогает при написании тестов, Вариативность, как говорит мой знакомый - при этом "на высоте" ;)

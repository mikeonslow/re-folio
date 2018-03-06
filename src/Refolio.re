let str = ReasonReact.stringToElement;

module Option = {
  let unwrapUnsafely = data =>
    switch data {
    | Some(v) => v
    | None => raise(Invalid_argument("unwrapUnsafely called on None"))
    };
};

type category = {
  id: int,
  label: string
};

type item = {
  id: int,
  title: string,
  categoryId: int,
  imageUrl: string,
  linkUrl: string,
  description: string,
  overlayColor: string
};

type portfolio = {
  categories: list(category),
  items: list(item)
};

module Decode = {
  let category = json =>
    Json.Decode.{
      id: json |> field("id", int),
      label: json |> field("label", string)
    };
  let item = json =>
    Json.Decode.{
      id: json |> field("id", int),
      title: json |> field("title", string),
      categoryId: json |> field("categoryId", int),
      imageUrl: json |> field("imageUrl", string),
      linkUrl: json |> field("linkUrl", string),
      description: json |> field("description", string),
      overlayColor: json |> field("overlayColor", string)
    };
  let portfolio = json =>
    Json.Decode.{
      categories: json |> field("categories", Json.Decode.list(category)),
      items: json |> field("items", Json.Decode.list(item))
    };
};

module CategoryButton = {
  let component = ReasonReact.statelessComponent("CategoryButton");
  let make = (~category, children) => {
    ...component,
    render: (_) => <button> (str(string_of_int(category.id))) </button>
  };
};

type remoteData =
  | NotAsked
  | Loading
  | Error
  | Success(portfolio);

type state = {
  errorMessage: option(string),
  portfolio: remoteData,
  selectedCategoryId: option(int),
  selectedItemId: option(int),
  apiUrl: string
};

type action =
  | FetchPortfolio(string)
  | FetchPortfolioFailure
  | FetchPortfolioSuccess(portfolio)
  | CategoryClicked(int)
  | ItemClicked(int);

let component = ReasonReact.reducerComponent("Refolio");

let make = children => {
  ...component,
  initialState: () => {
    errorMessage: None,
    portfolio: NotAsked,
    selectedCategoryId: None,
    selectedItemId: None,
    apiUrl: "http://www.mocky.io/v2/59f8cfa92d0000891dad41ed"
  },
  didMount: self => {
    self.send(FetchPortfolio(self.state.apiUrl));
    ReasonReact.NoUpdate;
  },
  reducer: (action, state) =>
    switch action {
    | FetchPortfolio(url) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, portfolio: Loading},
        (
          self =>
            Js.Promise.(
              Fetch.fetch(url)
              |> then_(Fetch.Response.json)
              |> then_(json =>
                   json
                   |> Decode.portfolio
                   |> (
                     portfolio => self.send(FetchPortfolioSuccess(portfolio))
                   )
                   |> resolve
                 )
              |> catch(_err =>
                   Js.Promise.resolve(self.send(FetchPortfolioFailure))
                 )
              |> ignore
            )
        )
      )
    | FetchPortfolioFailure => ReasonReact.NoUpdate
    | FetchPortfolioSuccess(portfolio) =>
      ReasonReact.Update({...state, portfolio: Success(portfolio)})
    /*(p => Js.log(portfolio.items))*/
    | CategoryClicked(id) => ReasonReact.NoUpdate
    | ItemClicked(id) => ReasonReact.NoUpdate
    },
  render: ({state: {apiUrl, portfolio}, reduce}) =>
    <div className="app">
      <h1 className="title"> (str("Refolio")) </h1>
      <div className="categories">
        (
          switch portfolio {
          | NotAsked => <div> (str("Not Asked")) </div>
          | Loading => <div> (str("Loading...")) </div>
          | Error => <div> (str("Error")) </div>
          | Success(portfolio) =>
            ReasonReact.arrayToElement(
              Array.of_list(
                List.map(
                  category =>
                    <CategoryButton
                      category
                      /*key=(string_of_int(category.id))*/
                    />,
                  portfolio.categories
                )
              )
            )
          }
        )
      </div>
      <div className="footer" />
    </div>
};

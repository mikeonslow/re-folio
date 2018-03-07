let str = ReasonReact.stringToElement;

let classList = classes =>
  classes
  |> List.map(((className, enabled)) => enabled ? className : "")
  |> String.concat(" ");

module Option = {
  let unwrapUnsafely = data =>
    switch data {
    | Some(v) => v
    | None => raise(Invalid_argument("unwrapUnsafely called on None"))
    };
};

module Category = {
  type t = {
    id: int,
    label: string
  };
};

module Item = {
  type t = {
    id: int,
    title: string,
    categoryId: int,
    imageUrl: string,
    linkUrl: string,
    description: string,
    overlayColor: string
  };
};

module Portfolio = {
  type t = {
    categories: list(Category.t),
    items: list(Item.t)
  };
};

module Decode = {
  let category = json =>
    Json.Decode.{
      Category.id: json |> field("id", int),
      label: json |> field("label", string)
    };
  let item = json =>
    Json.Decode.{
      Item.id: json |> field("id", int),
      title: json |> field("title", string),
      categoryId: json |> field("categoryId", int),
      imageUrl: json |> field("imageUrl", string),
      linkUrl: json |> field("linkUrl", string),
      description: json |> field("description", string),
      overlayColor: json |> field("overlayColor", string)
    };
  let portfolio = json =>
    Json.Decode.{
      Portfolio.categories:
        json |> field("categories", Json.Decode.list(category)),
      Portfolio.items: json |> field("items", Json.Decode.list(item))
    };
};

module CategoryButton = {
  let component = ReasonReact.statelessComponent("CategoryButton");
  let make = (~category: Category.t, ~selectedCategoryId, children) => {
    ...component,
    render: (_) =>
      <button
        className=(
          classList([
            ("btn btn-category", true),
            ("btn-primary", selectedCategoryId === category.id),
            ("btn-secondary", selectedCategoryId !== category.id)
          ])
        )>
        (str(category.label))
      </button>
  };
};

let categoryButtons = (categories, selectedCategoryId) =>
  categories
  |> List.map((category: Category.t) =>
       <CategoryButton
         key=(string_of_int(category.id))
         category
         selectedCategoryId
       />
     )
  |> Array.of_list
  |> ReasonReact.arrayToElement;

module CategoryNavbar = {
  let component = ReasonReact.statelessComponent("CategoryButton");
  let make = (~categories: list(Category.t), children) => {
    ...component,
    render: (_) =>
      <div className="row">
        <div className="col">
          <div className="nav-category">
            (categoryButtons(categories, 1))
          </div>
        </div>
      </div>
  };
};

module ItemView = {
  let component = ReasonReact.statelessComponent("ItemView");
  let make = (~category: Category.t, children) => {
    ...component,
    render: (_) =>
      <button className="btn btn-primary"> (str(category.label)) </button>
  };
};

type remoteData =
  | NotAsked
  | Loading
  | Error
  | Success(Portfolio.t);

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
  | FetchPortfolioSuccess(Portfolio.t)
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
                     portfolio => {
                       Js.log("portfolio.categories");
                       Js.log(portfolio.categories);
                       self.send(FetchPortfolioSuccess(portfolio));
                     }
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
    | CategoryClicked(id) => ReasonReact.NoUpdate
    | ItemClicked(id) => ReasonReact.NoUpdate
    },
  render: ({state: {apiUrl, portfolio}, reduce}) =>
    <div className="container">
      <div className="row"> <div className="col" /> </div>
      <div className="row">
        <div className="col"> <h1> (str("Re-folio")) </h1> </div>
      </div>
      <div className="row">
        <div className="col" />
        (
          switch portfolio {
          | NotAsked => <div> (str("Not Asked")) </div>
          | Loading => <div> (str("Loading...")) </div>
          | Error => <div> (str("Error")) </div>
          | Success(portfolio) =>
            <CategoryNavbar categories=portfolio.categories />
          }
        )
      </div>
      <div className="row"> <div className="col selected-item" /> </div>
      <div className="row"> <div className="col items" /> </div>
    </div>
};

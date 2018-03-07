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
  let component = ReasonReact.statelessComponent("Item");
  let make = (~item: t, ~selectedItemId: int, ~onClick, children) => {
    ...component,
    render: (_) =>
      <div className="col-4 item-panel">
        <img
          src=item.imageUrl
          className="img-fluid"
          onClick=(evt => onClick())
        />
      </div>
  };
};

module ItemsPane = {
  let component = ReasonReact.statelessComponent("ItemsPane");
  let make = (~items: list(Item.t), ~selectedItemId: int, ~onClick, children) => {
    ...component,
    render: (_) =>
      <div className="row">
        (
          items
          |> List.map((item: Item.t) =>
               <Item
                 key=(string_of_int(item.id))
                 item
                 selectedItemId
                 onClick=(evt => onClick())
               />
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>
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
      <div className="col nav-category">
        (categoryButtons(categories, 1))
      </div>
  };
};

module RemoteData = {
  type t =
    | NotAsked
    | Loading
    | Error(string)
    | Success(Portfolio.t);
  let defaultLoader =
    <div className="row">
      <div className="col-4 text-center" />
      <div className="col-4 remote-data-loader text-center">
        (str("Loading projects..."))
        <div className="fa-3x ">
          <i className="fas fa-circle-notch fa-spin" />
        </div>
      </div>
      <div className="col-4 text-center" />
    </div>;
  let initHandler = view => view;
  let loadingHandler = view => view;
  let errorHandler = (view, error) => view(error);
  let successHandler = (view, data) => view(data);
  let component = ReasonReact.statelessComponent("RemoteData");
  let make =
      (~state, ~initView, ~loadingView, ~errorView, ~successView, children) => {
    ...component,
    render: (_) =>
      switch state {
      | NotAsked => initHandler(initView)
      | Loading => loadingHandler(loadingView)
      | Error(error) => errorHandler(errorView, error)
      | Success(data) => successHandler(successView, data)
      }
  };
};

type state = {
  errorMessage: option(string),
  portfolio: RemoteData.t,
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
    apiUrl: "http://www.mocky.io/v2/59f8cfa92d0000891dad41ed?mocky-delay=1000ms"
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
  render: ({state: {portfolio}, reduce}) =>
    <div className="container">
      <div className="row"> <div className="col" /> </div>
      <div className="row">
        <div className="col"> <h1> (str("Re-folio")) </h1> </div>
      </div>
      <RemoteData
        state=portfolio
        initView=RemoteData.defaultLoader
        loadingView=RemoteData.defaultLoader
        errorView=(e => <div className="row"> (str(e)) </div>)
        successView=(
          data => {
            let categories = data.categories;
            let items = data.items;
            <div className="row">
              <CategoryNavbar categories />
              <ItemsPane
                items
                selectedItemId=1
                onClick=(reduce(evt => ItemClicked(2)))
              />
            </div>;
          }
        )
      />
    </div>
};

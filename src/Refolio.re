let str = ReasonReact.stringToElement;

let classList = classes =>
  classes
  |> List.map(((className, enabled)) => enabled ? className : "")
  |> String.concat(" ");

module Option = {
  let withDefault = (data, default) =>
    switch data {
    | Some(v) => v
    | None => default
    };
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
  let make = (~item: t, ~selectedItemId: int, ~onClick, _children) => {
    ...component,
    render: (_) =>
      <div className="col-md-4 item-panel">
        <a href="#refolio">
          <img
            src=item.imageUrl
            className="img-fluid"
            onClick=(_evt => selectedItemId === item.id ? () : onClick())
          />
        </a>
      </div>
  };
};

module Portfolio = {
  type t = {
    categories: list(Category.t),
    items: list(Item.t)
  };
};

type action =
  | FetchPortfolio(string)
  | FetchPortfolioFailure
  | FetchPortfolioSuccess(Portfolio.t)
  | CategoryClicked(int)
  | ItemClicked(int);

module ItemsPane = {
  let component = ReasonReact.statelessComponent("ItemsPane");
  let make = (~items: list(Item.t), ~selectedItemId: int, ~onClick, _children) => {
    ...component,
    render: (_) =>
      <div className="row items-container">
        (
          items
          |> List.map((item: Item.t) =>
               <Item
                 key=(string_of_int(item.id))
                 item
                 selectedItemId
                 onClick=(_evt => onClick(item.id))
               />
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>
  };
};

module SelectedItem = {
  let component = ReasonReact.statelessComponent("SelectedItem");
  let make = (~items: list(Item.t), ~selectedItemId: int, _children) => {
    ...component,
    render: (_) => {
      let itemList =
        items |> List.filter((item: Item.t) => item.id == selectedItemId);
      switch itemList {
      | [] => <div className="row selected-item-no-match" />
      | [detail] =>
        <div className="row selected-item-container">
          <div className="col-md-6">
            <img className="img-fluid" src=detail.imageUrl />
          </div>
          <div className="col-md-6">
            <h3> (str(detail.title)) </h3>
            <hr />
            <span> (str(detail.description)) </span>
            <br />
            <br />
            <a href=detail.linkUrl target="_blank"> (str(detail.linkUrl)) </a>
          </div>
        </div>
      | [_a, ..._tail] =>
        <div className="row selected-item-duplicate-matches">
          <div className="col">
            (str("Error: Duplicate items with same ID exist!!!"))
          </div>
        </div>
      };
    }
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
  let make = (~category: Category.t, ~selectedCategoryId, ~onClick, _children) => {
    ...component,
    render: (_) =>
      <button
        className=(
          classList([
            ("btn btn-category", true),
            ("btn-primary", selectedCategoryId === category.id),
            ("btn-secondary", selectedCategoryId !== category.id)
          ])
        )
        onClick=(_evt => onClick(category.id))>
        (str(category.label))
      </button>
  };
};

let categoryButtons = (categories, selectedCategoryId, onClick) =>
  categories
  |> List.map((category: Category.t) =>
       <CategoryButton
         key=(string_of_int(category.id))
         category
         selectedCategoryId
         onClick
       />
     )
  |> Array.of_list
  |> ReasonReact.arrayToElement;

module CategoryNavbar = {
  let component = ReasonReact.statelessComponent("CategoryButton");
  let make =
      (~categories: list(Category.t), ~selectedCategoryId, ~onClick, _children) => {
    ...component,
    render: (_) =>
      <div className="row">
        <div className="col nav-category">
          (categoryButtons(categories, selectedCategoryId, onClick))
        </div>
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
      <div className="col-md-4" />
      <div className="col-md-4 remote-data-loader text-center">
        (str("Loading projects..."))
        <div className="fa-3x">
          <i className="fas fa-circle-notch fa-spin" />
        </div>
      </div>
      <div className="col-md-4" />
    </div>;
  let defaultError = error =>
    <div className="row">
      <div className="col-md-4" />
      <div className="col-md-4 remote-data-error text-center">
        <div className="fa-3x ">
          <i className="fas fa-exclamation-triangle" />
        </div>
        (str(error))
      </div>
      <div className="col-md-4" />
    </div>;
  let initHandler = view => view;
  let loadingHandler = view => view;
  let errorHandler = (view, error) => view(error);
  let successHandler = (view, data) => view(data);
  let component = ReasonReact.statelessComponent("RemoteData");
  let make =
      (~state, ~initView, ~loadingView, ~errorView, ~successView, _children) => {
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

let component = ReasonReact.reducerComponent("Refolio");

let make = _children => {
  ...component,
  initialState: () => {
    errorMessage: None,
    portfolio: NotAsked,
    selectedCategoryId: None,
    selectedItemId: None,
    apiUrl: "https://www.mocky.io/v2/59f8cfa92d0000891dad41ed"
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
    | FetchPortfolioFailure =>
      ReasonReact.Update({
        ...state,
        portfolio: Error("Failed to fetch projects from API...")
      })
    | FetchPortfolioSuccess(portfolio) =>
      let defaultSelectedCategory = () =>
        switch portfolio.categories {
        | [] => None
        | [a] => Some(a.id)
        | [a, ..._tail] => Some(a.id)
        };
      ReasonReact.Update({
        ...state,
        selectedCategoryId: defaultSelectedCategory(),
        portfolio: Success(portfolio)
      });
    | CategoryClicked(id) =>
      ReasonReact.Update({
        ...state,
        selectedCategoryId: Some(id),
        selectedItemId: None
      })
    | ItemClicked(id) =>
      ReasonReact.Update({...state, selectedItemId: Some(id)})
    },
  render: ({state: {portfolio, selectedCategoryId, selectedItemId}, reduce}) => {
    let categoryId = Option.withDefault(selectedCategoryId, 0);
    let itemId = Option.withDefault(selectedItemId, 0);
    <div id="refolio" className="container">
      <div className="row"> <div className="col"> <br /> </div> </div>
      <div className="row">
        <div className="col"> <h1> (str("Re-folio")) </h1> </div>
      </div>
      <RemoteData
        state=portfolio
        initView=RemoteData.defaultLoader
        loadingView=RemoteData.defaultLoader
        errorView=(e => RemoteData.defaultError(e))
        successView=(
          data => {
            let categories = data.categories;
            let items =
              data.items
              |> List.filter((i: Item.t) =>
                   0 === categoryId || i.categoryId == categoryId
                 );
            <span>
              <CategoryNavbar
                categories
                selectedCategoryId=categoryId
                onClick=(reduce(id => CategoryClicked(id)))
              />
              <SelectedItem items selectedItemId=itemId />
              <ItemsPane
                items
                selectedItemId=itemId
                onClick=(reduce(id => ItemClicked(id)))
              />
            </span>;
          }
        )
      />
    </div>;
  }
};

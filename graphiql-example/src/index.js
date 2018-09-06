import { parse } from 'graphql';
import ApolloClient from "apollo-client";
import { WebSocketLink } from "apollo-link-ws";
import { SubscriptionClient } from 'subscriptions-transport-ws';
import {InMemoryCache} from 'apollo-cache-inmemory';
import * as React from 'react';
import * as ReactDOM from 'react-dom';
import GraphiQL from 'graphiql';
import gql from 'graphql-tag';

/**
 * This GraphiQL example illustrates how to use some of GraphiQL's props
 * in order to enable reading and updating the URL parameters, making
 * link sharing of queries a little bit easier.
 *
 * This is only one example of this kind of feature, GraphiQL exposes
 * various React params to enable interesting integrations.
 */

// Parse the search string to get url parameters.
var search = window.location.search;
var parameters = {};
search.substr(1).split('&').forEach(function (entry) {
  var eq = entry.indexOf('=');
  if (eq >= 0) {
    parameters[decodeURIComponent(entry.slice(0, eq))] =
      decodeURIComponent(entry.slice(eq + 1));
  }
});

// if variables was provided, try to format it.
if (parameters.variables) {
  try {
    parameters.variables =
      JSON.stringify(JSON.parse(parameters.variables), null, 2);
  } catch (e) {
    // Do nothing, we want to display the invalid JSON as a string, rather
    // than present an error.
  }
}

// When the query and variables string is edited, update the URL bar so
// that it can be easily shared
function onEditQuery(newQuery) {
  parameters.query = newQuery;
  updateURL();
}

function onEditVariables(newVariables) {
  parameters.variables = newVariables;
  updateURL();
}

function onEditOperationName(newOperationName) {
  parameters.operationName = newOperationName;
  updateURL();
}

function updateURL() {
  var newSearch = '?' + Object.keys(parameters).filter(function (key) {
    return Boolean(parameters[key]);
  }).map(function (key) {
    return encodeURIComponent(key) + '=' +
      encodeURIComponent(parameters[key]);
  }).join('&');
  history.replaceState(null, null, newSearch);
}

// Defines a GraphQL fetcher using the fetch API. You're not required to
// use fetch, and could instead implement graphQLFetcher however you like,
// as long as it returns a Promise or Observable.
// export function graphQLFetcher(graphQLParams) {
//   // This example expects a GraphQL server at the path /graphql.
//   // Change this to point wherever you host your GraphQL server.
//   return fetch('/graphql', {
//     method: 'post',
//     headers: {
//       'Accept': 'application/json',
//       'Content-Type': 'application/json',
//     },
//     body: JSON.stringify(graphQLParams),
//     credentials: 'include',
//   }).then(function (response) {
//     return response.text();
//   }).then(function (responseBody) {
//     try {
//       return JSON.parse(responseBody);
//     } catch (error) {
//       return responseBody;
//     }
//   });
// }
const GRAPHQL_ENDPOINT = 'ws://localhost:9160';

const client = new SubscriptionClient(GRAPHQL_ENDPOINT, {
  reconnect: true,
});
const link = new WebSocketLink(client);

// addTypename needs to be false b/c __typename is not yet supported by
// graphql-api
const apolloClient = new ApolloClient({
  link: link,
  cache: new InMemoryCache({
    addTypename: false
  }),
  connectToDevTools: true,
  addTypename: false
});

export function apolloFetcher(graphQLParams) {
  // This example expects a GraphQL server at the path /graphql.
  // Change this to point wherever you host your GraphQL server.
  const qDoc = parse(graphQLParams.query, {noLocation: true});
  console.log("graphQLParams are", graphQLParams);
  console.log("parsed query is", qDoc);
  console.log(link);
  if (hasSubscriptionOperation(graphQLParams)) {
    let subObs = apolloClient.subscribe({
      query: parse(graphQLParams.query, {noLocation: true}),
      fetchPolicy: 'no-cache'
    });
    // subObs.subscribe(function (response) {
    //   console.log("Got response!", response);
    //   return response;
    // }, function (error) {
    //   console.log("Error from apolloClient.parse", error);
    // });
    return subObs;
  } else {
    let promise = apolloClient.query({
      query: parse(graphQLParams.query, {noLocation: true}),
      fetchPolicy: 'no-cache'
    }).then(function (response) {
      console.log("Got response!", response);
      return response;
    }, function (error) {
      console.log("Error from apolloClient.parse", error);
    });
    return promise;
  }
  // .then(function (responseBody) {
  //   try {
  //     return JSON.parse(responseBody);
  //   } catch (error) {
  //     return responseBody;
  //   }
  // }, function(error) {
  //   console.log("Error somewhere else", error);
  // });
  //   method: 'post',
  //   headers: {
  //     'Accept': 'application/json',
  //     'Content-Type': 'application/json',
  //   },
  //   body: JSON.stringify(graphQLParams),
  //   credentials: 'include',
  // }).then(function (response) {
  //   return response.text();
  // }).then(function (responseBody) {
  //   try {
  //     return JSON.parse(responseBody);
  //   } catch (error) {
  //     return responseBody;
  //   }
  // });
}

// let subscriptionsClient = new window.SubscriptionsTransportWs.SubscriptionClient('ws://localhost:9160', {
//   reconnect: true
// });
// subscriptionsClient.subscribe = subscriptionsClient.request;
// let myCustomFetcher = window.GraphiQLSubscriptionsFetcher.graphQLFetcher(
//   subscriptionsClient, graphQLFetcher);

// Render <GraphiQL /> into the body.
// See the README in the top level of this module to learn more about
// how you can customize GraphiQL by providing different values or
// additional child elements.

const hasSubscriptionOperation = (graphQlParams) => {
  const queryDoc = parse(graphQlParams.query);

  for (let definition of queryDoc.definitions) {
    if (definition.kind === 'OperationDefinition') {
      const operation = definition.operation;
      if (operation === 'subscription') {
        return true;
      }
    }
  }

  return false;
};

ReactDOM.render(
  React.createElement(GraphiQL, {
    fetcher: apolloFetcher,
    schema: null,
    query: parameters.query,
    variables: parameters.variables,
    operationName: parameters.operationName,
    onEditQuery: onEditQuery,
    onEditVariables: onEditVariables,
    onEditOperationName: onEditOperationName
  }),
  document.getElementById('graphiql')
);

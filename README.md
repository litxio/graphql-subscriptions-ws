# graphql-subscriptions-ws

Proof of concept for using GraphQL subscriptions with the [graphql-api](https://github.com/haskell-graphql/graphql-api) library.

This package is largely a port of the [subscriptions-transport-ws](https://github.com/apollographql/subscriptions-transport-ws) JavaScript/TypeScript library to Haskell.

It also comes with an example GraphiQL app in the `graphiql-example` directory.

## Building and running

```
stack build
stack exec -- graphql-subscriptions-ws
```

To run the GraphiQL frontend, in the `graphiql-example` directory:

```
npm install
npm run build
npm run start
```

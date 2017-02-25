// @flow

/**
 * Types that Relay framework users may find useful.
 */
// import type RelayFragmentReference from 'RelayFragmentReference';
type RelayFragmentReference = any;
// import type {
//   DataID,
//   FieldValue,
//   RangeBehaviors,
// } from 'RelayInternalTypes';
type DataID = any;
type FieldValue = any;
type RangeBehaviors = any;
// import type RelayMutation from 'RelayMutation';
// import type RelayMutationRequest from 'RelayMutationRequest';
type RelayMutationRequest = any;
// import type RelayMutationTransaction from 'RelayMutationTransaction';
type RelayMutationTransaction = any;
// import type {RelayConcreteNode} from 'RelayQL';
type RelayConcreteNode = any;
// import type {RelayQueryConfigInterface} from 'RelayQueryConfig';
type RelayQueryConfigInterface = any;
// import type RelayQueryRequest from 'RelayQueryRequest';
type RelayQueryRequest = any;
// import type {Record} from 'RelayRecord';
type Record = any;
// import type URI from 'URI';
type URI = any;

type RelayContainerErrorEventType = (
  'CACHE_RESTORE_FAILED' |
  'NETWORK_QUERY_ERROR'
);
type RelayContainerLoadingEventType = (
  'ABORT' |
  'CACHE_RESTORED_REQUIRED' |
  'CACHE_RESTORE_START' |
  'NETWORK_QUERY_RECEIVED_ALL' |
  'NETWORK_QUERY_RECEIVED_REQUIRED' |
  'NETWORK_QUERY_START' |
  'STORE_FOUND_ALL' |
  'STORE_FOUND_REQUIRED'
);
type XHRErrorData = {
  errorCode: ?string,
  errorMsg: ?string,
  errorType: ?string,
};

// Utility
type Abortable = {
  abort(): void,
};
// Disk Cache
type CacheManager = {
  clear(): void,
  getMutationWriter(): CacheWriter,
  getQueryWriter(): CacheWriter,
  readNode(
    id: DataID,
    callback: (error: any, value: any) => void
  ): void,
   readRootCall(
    callName: string,
    callValue: string,
    callback: (error: any, value: any) => void
  ): void,
};
type CacheProcessorCallbacks = {
  +onSuccess?: () => void,
  +onFailure?: () => void,
};
type CacheWriter = {
  writeField(
    dataID: DataID,
    field: string,
    value: ?FieldValue,
    typeName: ?string
  ): void,
  writeNode(dataID: DataID, record: ?Record): void,
  writeRootCall(
    storageKey: string,
    identifyingArgValue: string,
    dataID: DataID
  ): void,
};
// Store Change Emitter
type ChangeSubscription = {
  remove(): void,
};
type ComponentFetchState = {
  done: boolean,
  stale: boolean,
};
// Ready State
type ComponentReadyState = {
  aborted: boolean,
  done: boolean,
  error: ?Error,
  events: Array<ReadyStateEvent>,
  mounted: boolean,
  ready: boolean,
  stale: boolean,
};
type ComponentReadyStateChangeCallback =
  (readyState: ComponentReadyState) => void;
type MultiObservable<T> = {
  subscribe(callbacks: SubscriptionCallbacks<Array<T>>): Subscription,
  setDataIDs(dataIDs: Array<DataID>): void,
};
type MutationResult = {
  response: Object,
};
// Network requests
type NetworkLayer = {
  sendMutation(request: RelayMutationRequest): ?Promise<any>,
  sendQueries(requests: Array<RelayQueryRequest>): ?Promise<any>,
  supports(...options: Array<string>): boolean,
};
// Observable
type Observable<T> = {
  subscribe(callbacks: SubscriptionCallbacks<T>): Subscription,
};
type QueryResult = {
  error?: ?Error,
  ref_params?: ?{[name: string]: mixed},
  response: Object,
};
type ReadyState = {
  aborted: boolean,
  done: boolean,
  error: ?Error,
  events: Array<ReadyStateEvent>,
  ready: boolean,
  stale: boolean,
};
type ReadyStateChangeCallback = (readyState: ReadyState) => void;
type ReadyStateEvent = {
  type: RelayContainerLoadingEventType | RelayContainerErrorEventType,
  error?: Error,
}
// Containers
type RelayContainer = ReactClass<any>;
type RelayMutationConfig = {
  type: 'FIELDS_CHANGE',
  fieldIDs: {[fieldName: string]: DataID | Array<DataID>},
} | {
  type: 'RANGE_ADD',
  parentName: string,
  parentID?: string,
  connectionName: string,
  edgeName: string,
  rangeBehaviors: RangeBehaviors,
} | {
  type: 'NODE_DELETE',
  parentName: string,
  parentID?: string,
  connectionName: string,
  deletedIDFieldName: string,
} | {
  type: 'RANGE_DELETE',
  parentName: string,
  parentID?: string,
  connectionName: string,
  deletedIDFieldName: string | Array<string>,
  pathToConnection: Array<string>,
} | {
  type: 'REQUIRED_CHILDREN',
  children: Array<RelayConcreteNode>,
};
type RelayMutationTransactionCommitCallbacks = {
  onFailure?: ?RelayMutationTransactionCommitFailureCallback,
  onSuccess?: ?RelayMutationTransactionCommitSuccessCallback,
};
// Mutations
type RelayMutationTransactionCommitFailureCallback = (
  transaction: RelayMutationTransaction,
  preventAutoRollback: () => void,
) => void;
type RelayMutationTransactionCommitSuccessCallback = (
  response: {[key: string]: Object}
) => void;
type RelayProp = {
  applyUpdate: (
    mutation: RelayMutation<any>,
    callbacks?: RelayMutationTransactionCommitCallbacks
  ) => RelayMutationTransaction,
  commitUpdate: (
    mutation: RelayMutation<any>,
    callbacks?: RelayMutationTransactionCommitCallbacks
  ) => RelayMutationTransaction,
  forceFetch: (
    partialVariables?: ?Variables,
    callback?: ?ComponentReadyStateChangeCallback
  ) => void,
  getPendingTransactions(record: Object): ?Array<RelayMutationTransaction>,
  hasFragmentData: (
    fragmentReference: RelayFragmentReference,
    record: Object
  ) => boolean,
  hasOptimisticUpdate: (
    record: Object
  ) => boolean,
  hasPartialData: (
    record: Object
  ) => boolean,
  pendingVariables: ?Variables,
  route: RelayQueryConfigInterface,
  setVariables: (
    partialVariables?: ?Variables,
    callback?: ?ComponentReadyStateChangeCallback
  ) => void,
  variables: Variables,
};
type RequestOptions = {
  data?: ?{[key: string]: mixed},
  errorHandler?: ?(error: XHRErrorData) => void,
  headers?: ?{[key: string]: string},
  method: string,
  rawData?: mixed,
  responseHandler?: ?(
    responseText: string,
    responseHeaders: ?string,
    isComplete: boolean
  ) => void,
  timeout?: ?number,
  timeoutHandler?: ?() => void,
  transportBuilder?: any,
  uri: URI,
};
// Store
type StoreReaderData = Object;
type StoreReaderOptions = {
  traverseFragmentReferences?: boolean,
  traverseGeneratedFields?: boolean,
};
type Subscription = {
  dispose(): void,
};
type SubscriptionCallbacks<T> = {
  onNext(value: T): void,
  onError(error: Error): void,
  onCompleted(): void,
};
// Variables
export type Variables = {[name: string]: mixed};

/*
 * Relay.Mutation
 */

type RelayFragmentsData<O> = $ObjMap<O, <T>(() => T) => $GraphqlData<T>>;

declare class RelayMutation<Mutation, Frags> {
  static fragments: Frags;
  props: RelayFragmentsData<Frags>;

  getMutation(): Mutation;
  getVariables():
    $Subtype<
      $Diff<
        $PropertyType<$GraphqlVars<Mutation>, 'input'>,
        {clientMutationId: string},
      >
    >;
}

/*
 * Relay.createComponent
 */

type RelayComponent<FragDefs> = Class<
  React$Component<*, {relay: RelayProp} & RelayFragmentsData<FragDefs>, *>
>;

type UnwrapRef<T> = (() => T) => 'id';
type RelayClass<FragDefs> = Class<
  React$Component<void, $ObjMap<FragDefs, UnwrapRef<*>>, void>
>;


declare module 'react-relay' {
  declare var Environment: any;
  declare var GraphQLMutation: any;
  declare var Mutation: typeof RelayMutation;
  // declare var PropTypes: any;
  // QL: RelayQL;
  declare var QueryConfig: any;
  declare var ReadyStateRenderer: any;
  declare var Renderer: any;
  declare var RootContainer: any;
  declare var Route: any;
  declare var Store: any;

  declare function createContainer<Frags, Comp: RelayComponent<Frags>>(
    Comp, {fragments: Frags},
  ): RelayClass<Frags>;
  declare var createQuery: any;
  declare var getQueries: any;
  declare var disableQueryCaching: any;
  declare var injectNetworkLayer: any;
  declare var injectTaskScheduler: any;
  declare var isContainer: any;
}

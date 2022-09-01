type auth = {
  api_key : string;
  secret : string;
}

type client

val create : client:Request.client -> auth:auth -> client

include Types.Provider with type client := client

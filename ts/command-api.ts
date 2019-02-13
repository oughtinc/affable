import axios from "axios";
import { Workspace, Message, Either, Pointer, FetchResult, Result } from "./types";

export function postView(body: [{userId: number}, number, Array<Message>, Pointer]): Promise<FetchResult<Result>>
{
  return axios({ url: '/view'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postReply(body: [{userId: number}, number, Array<Either<Message, Pointer>>, Message]): Promise<FetchResult<Result>>
{
  return axios({ url: '/reply'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postWait(body: [{userId: number}, number, Array<Either<Message, Pointer>>]): Promise<FetchResult<Result>>
{
  return axios({ url: '/wait'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postNext(body: {userId: number}): Promise<FetchResult<Workspace | null>>
{
  return axios({ url: '/next'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function getJoin(): Promise<FetchResult<{userId: number}>>
{
  return axios({ url: '/join'
    , method: 'get'
    });
}

export function getPointer(p: Pointer): Promise<FetchResult<Message | null>>
{
  return axios({ url: '/pointer/' + encodeURIComponent(''+p) + ''
    , method: 'get'
    });
}

import axios from "axios";
import { WorkspaceRaw, Message, Either, Pointer, FetchResult, Result } from "./types";

export function postView(body: [{userId: number}, number, Array<Message>, Pointer]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/view'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postReply(body: [{userId: number}, number, Array<Either<Message, Pointer>>, Message]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/reply'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postWait(body: [{userId: number}, number, Array<Either<Message, Pointer>>]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/wait'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postNext(body: [{userId: number}, number | null]): Promise<FetchResult<[WorkspaceRaw, number] | null>>
{
  return axios({ url: '/next'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function getJoin(userId: number): Promise<FetchResult<{userId: number}>>
{
  return axios({ url: '/join' + (isNaN(userId) ? '' : '?userId=' + encodeURIComponent(''+userId))
    , method: 'get'
    });
}

export function getPointer(p: Pointer): Promise<FetchResult<Message | null>>
{
  return axios({ url: '/pointer/' + encodeURIComponent(''+p) + ''
    , method: 'get'
    });
}

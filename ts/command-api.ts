import axios from "axios";
import { WorkspaceRaw, Message, Either, Pointer, FetchResult, Result } from "./types";

export function postView(body: [{userId: string}, number, Array<Message>, Pointer]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/view'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postReply(body: [{userId: string}, number, Array<Either<Message, Pointer>>, Message]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/reply'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postWait(body: [{userId: string}, number, Array<Either<Message, Pointer>>]): Promise<FetchResult<Result<void>>>
{
  return axios({ url: '/wait'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function postNext(body: [{userId: string}, string | null]): Promise<FetchResult<[WorkspaceRaw, string] | null>>
{
  return axios({ url: '/next'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

export function getJoin(userId: string | undefined): Promise<FetchResult<{userId: string}>>
{
  return axios({ url: '/join' + (userId === void(0) ? '' : '?userId=' + encodeURIComponent(userId))
    , method: 'get'
    });
}

export function getPointer(p: Pointer): Promise<FetchResult<Message | null>>
{
  return axios({ url: '/pointer/' + encodeURIComponent(''+p) + ''
    , method: 'get'
    });
}

export function getCompletions(sessionId: string): Promise<FetchResult<Array<Message>>>
{
  return axios({ url: '/completions/' + encodeURIComponent(sessionId) + ''
    , method: 'get'
    });
}

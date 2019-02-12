var postView = function(body)
{
  return axios({ url: '/view'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

var postReply = function(body)
{
  return axios({ url: '/reply'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

var postWait = function(body)
{
  return axios({ url: '/wait'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

var postNext = function(body)
{
  return axios({ url: '/next'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}

var getJoin = function()
{
  return axios({ url: '/join'
    , method: 'get'
    });
}

var getPointer = function(p)
{
  return axios({ url: '/pointer/' + encodeURIComponent(p) + ''
    , method: 'get'
    });
}

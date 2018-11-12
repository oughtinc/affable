
var postCommand = function(body)
{
  return axios({ url: '/command'
    , method: 'post'
    , data: body
    , responseType: 'json'
    });
}



var getTest = function()
{
  return axios({ url: '/test'
    , method: 'get'
    });
}

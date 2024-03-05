syntax match Constant    /\v[+-]?\d+(\.\d+)?(\ \w+)?/
syntax match Comment     /--.*$/
syntax match Title       /^\d\d\d\d-\d\d-\d\d/
syntax match Keyword     /\V+ tag/
syntax match Keyword     /\V+ assert/
syntax match Keyword     /\V+ attach/
syntax match Keyword     /\Vimport/
syntax match Keyword     /\Vaccount/
syntax match Keyword     /\Vtag/
syntax match Keyword     /\Vcurrency/
syntax match Keyword     /\Vprice/
syntax match Keyword     /\v\*/
syntax match Keyword     /\v\!/
syntax match Type        /\v(\w|[_-])+(:(\w|[_-])+)+/
syntax match Identifier  /\v\|.*/
syntax match String      /\v(\w|[_-]|\.)+(\/(\w|[_-]|\.)+)+/

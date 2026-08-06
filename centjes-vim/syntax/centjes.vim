" Vim syntax file for the centjes DSL.
"
" Every lexical element the lexer produces should be highlighted here, in the
" contexts where the lexer produces it and no others.  Adding a token to
" Centjes.Parse.Alex therefore means adding it to the corpus in
" `test_resources/` as well, which is where that claim is pinned down.
"
" Each declaration is a chain: a line-anchored item names the declaration, and
" `nextgroup` says what may follow.  Everything a chain reaches is `contained`,
" so a word like `lot` or `tag` highlights where the lexer would produce that
" token and stays plain text inside a description or an account name.
"
" The chains are more permissive than the grammar about what may follow what:
" one currency-symbol item serves every position a currency symbol can appear
" in, and its `nextgroup` is the union over those positions.  Over-permissive
" continuation only ever mis-highlights input that does not parse, whereas one
" item per position would multiply near-identical items.
"
" Definition order is load-bearing: where two items can match at the same
" position, vim gives it to the one defined later, so items go from most
" general to most specific.  `currency` and the virtual scopes sit exactly
" where an assertion's account name would, and only win by being defined after
" it -- which is what the lexer does too, by preferring its longest match.

if exists("b:current_syntax")
  finish
endif

syntax case match

" A comment is a line of its own, indented within a declaration or at the first
" column as a declaration.  Anchoring it is what keeps `attach a--b.pdf` whole.
syntax match centjesComment /^\s*\zs--.*$/

syntax match centjesImportKeyword /^\s*\zsimport\>/
      \ nextgroup=centjesFilePath skipwhite

syntax match centjesCurrencyKeyword /^\s*\zscurrency\>/
      \ nextgroup=centjesCurrencySymbol skipwhite

syntax match centjesAccountKeyword /^\s*\zsaccount\>/
      \ nextgroup=centjesAccountName skipwhite

syntax match centjesTagKeyword /^\s*\zstag\>/
      \ nextgroup=centjesTagName skipwhite

syntax match centjesPriceKeyword /^\s*\zsprice\>/
      \ nextgroup=centjesPriceTimestamp skipwhite

syntax match centjesTimestamp /^\s*\zs\d\{4}-\d\d-\d\d\%(\s\d\d:\d\d\%(:\d\d\)\?\)\?/

syntax match centjesDescription /^\s*\zs|.*$/

syntax match centjesPostingKeyword /^\s*\zs[*!]/
      \ nextgroup=centjesAccountName skipwhite

syntax match centjesExtraKeyword /^\s*\zs+/
      \ nextgroup=centjesAssertKeyword,centjesAssertVirtualKeyword,centjesAttachKeyword,centjesExtraTagKeyword
      \ skipwhite

" A file path is any run of non-newline characters, so it reaches end of line.
syntax match centjesFilePath /\S.*$/ contained

syntax match centjesPriceTimestamp /\d\{4}-\d\d-\d\d\%(\s\d\d:\d\d\%(:\d\d\)\?\)\?/ contained
      \ nextgroup=centjesCurrencySymbol skipwhite

syntax match centjesAccountName /\a[[:alnum:]_:-]*/ contained
      \ nextgroup=centjesAccountType,centjesNumber,centjesEqualsOperator skipwhite

syntax match centjesTagName /\a[[:alnum:]_:-]*/ contained

syntax match centjesCurrencySymbol /\a[[:alnum:]_:-]*/ contained
      \ nextgroup=centjesNumber,centjesLotKeyword,centjesCostOperator,centjesRatioOperator
      \ skipwhite

syntax match centjesNumber /[+-]\?\d\+\%(\.\d\+\)\?/ contained
      \ nextgroup=centjesCurrencySymbol,centjesFractionOperator,centjesPercentOperator
      \ skipwhite

syntax match centjesEqualsOperator /=/ contained
      \ nextgroup=centjesNumber skipwhite

syntax match centjesCostOperator /@/ contained
      \ nextgroup=centjesNumber skipwhite

syntax match centjesFractionOperator "/" contained
      \ nextgroup=centjesNumber skipwhite

syntax match centjesRatioOperator /\~[ie]\?[udn]\?/ contained
      \ nextgroup=centjesNumber skipwhite

syntax match centjesPercentOperator /%/ contained

syntax match centjesAttachKeyword /attach\>/ contained
      \ nextgroup=centjesFilePath skipwhite

syntax match centjesExtraTagKeyword /tag\>/ contained
      \ nextgroup=centjesTagName skipwhite

syntax match centjesLotKeyword /lot\>/ contained
      \ nextgroup=centjesCostOperator skipwhite

syntax match centjesAccountType /\%(assets\|liabilities\|equity\|expenses\|income\|other\)\>/ contained

syntax match centjesAssertCurrencyKeyword /currency\>/ contained
      \ nextgroup=centjesCurrencySymbol skipwhite

syntax match centjesVirtualScope /\%(no-virtual\|virtual-allowed\|virtual-only\)\>/ contained

" The scope keywords need the space the lexer's `assert virtual ` ends in:
" without it, `assert virtual-allowed` would take the scoped form and leave a
" stray `-allowed`, and the plain form would refuse an input it does own.
syntax match centjesAssertKeyword /assert\>\%( virtual \)\@!/ contained
      \ nextgroup=centjesAssertCurrencyKeyword,centjesVirtualScope,centjesAccountName
      \ skipwhite

syntax match centjesAssertVirtualKeyword /assert virtual\ze / contained
      \ nextgroup=centjesAccountName skipwhite

highlight def link centjesComment Comment
highlight def link centjesImportKeyword Keyword
highlight def link centjesCurrencyKeyword Keyword
highlight def link centjesAccountKeyword Keyword
highlight def link centjesTagKeyword Keyword
highlight def link centjesPriceKeyword Keyword
highlight def link centjesPostingKeyword Keyword
highlight def link centjesExtraKeyword Keyword
highlight def link centjesAssertKeyword Keyword
highlight def link centjesAssertVirtualKeyword Keyword
highlight def link centjesAssertCurrencyKeyword Keyword
highlight def link centjesVirtualScope Keyword
highlight def link centjesAttachKeyword Keyword
highlight def link centjesExtraTagKeyword Keyword
highlight def link centjesLotKeyword Keyword
highlight def link centjesAccountType Keyword
highlight def link centjesTimestamp Title
highlight def link centjesPriceTimestamp Title
highlight def link centjesAccountName Type
highlight def link centjesDescription Identifier
highlight def link centjesTagName Identifier
highlight def link centjesFilePath String
highlight def link centjesCurrencySymbol Constant
highlight def link centjesNumber Constant

" `@`, `~` and `=` each join two things and decide what the amount beside them
" means, so they read as the markers they are.  `/` and `%` are internal to a
" single rational value -- `1 / 7` and `10%` are each one number, the way the
" grammar has them -- so they read as part of it instead.
highlight def link centjesCostOperator Keyword
highlight def link centjesRatioOperator Keyword
highlight def link centjesEqualsOperator Keyword
highlight def link centjesFractionOperator Constant
highlight def link centjesPercentOperator Constant

let b:current_syntax = "centjes"

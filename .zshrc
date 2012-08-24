## 基本環境変数 
export EDITOR=emacsclient
export HTTP_HOME=http://www.google.co.jp/
# LANG設定
case ${UID} in 
0) #rootユーザ用
LANG=C
#LANG=en_US.UTF-8
  ;;
*) #一般ユーザ用
#export LANG=ja_JP.UTF-8
;; 
esac
# PATH設定
for p in ~/bin /usr/bin/gnu /usr/local/bin /usr/bin/gnu/gzip ; do
	[[ -d $p/. ]] || continue
	[[ :$PATH: = *:$p:* ]] || PATH=$p:$PATH
done
export PATH

# zsh系環境変数設定
WORDCHARS=${WORDCHARS:s,/,,} # 「/」も単語区切りとみなす。
REPORTTIME=3 # 実行したプロセスの消費時間が3秒以上かかったら自動的に消費時間の統計情報を表示する。

#色設定をエスケープではなく変数で指定できるように
autoload colors
colors

#履歴の設定
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt share_history # コマンド履歴を共有する
setopt HIST_IGNORE_ALL_DUPS # 同一コマンドヒストリの場合に、古いものを削除
setopt hist_save_no_dups # 重複するコマンドが保存される時、古い方を削除する。 
setopt hist_expire_dups_first	# 古い履歴を削除する必要がある場合、まず重複しているものから削除する。 
setopt hist_find_no_dups	# 履歴検索で重複しているものを表示しない。 
setopt append_history # 履歴を上書きしないで追加する。 
setopt hist_no_store # historyコマンドは除去する。 
setopt HIST_IGNORE_SPACE # ヒストリ追加時に先頭余白削除
setopt HIST_REDUCE_BLANKS # ヒストリ追加時に余白削除

#- 履歴検索機能のショートカット設定 C-P/N で履歴検索
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## alias の設定 
#ls -F:フォルダには/ --color:カラー
#汎用
#alias ls='ls -F --color' #ash に -F --colorはねぇ
#alias la='ls -laF --color'
#alias rm='rm -i' 
alias du='du -h' 
alias df='df -h' 
alias his='history -i -D'
alias ping='ping -c 5'
alias lv='lv -Au8'
alias screen='SHELL=/usr/bin/zsh screen -U'
alias sc='[ ${STY} ] || screen -U -rx || screen -U -D -RR'
alias grep-find='find . -type f -print0 | xargs grep -nH -e'
#固有
alias howm='emacs -nw -e howm-menu'
alias navi2ch='emacs -nw -e navi2ch'

sudo(){ devel-su -c "cd `pwd`;$*" }

#global alias
alias -g U=' --help | head' #usage
alias -g P=' --help | lv' #helP
alias -g L='| lv' #Lv
alias -g H='| head' #Head
alias -g T='| tail' #Tail
alias -g G='| grep' #Grep
#alias -g V='| vim -R -'

## 自動補完
autoload -Uz compinit
compinit
autoload predict-on # 履歴による先方予測補完 
predict-on 
zle -N predict-on
zle -N predict-off
bindkey '^xp' predict-on
bindkey '^x^p' predict-off
zstyle ':predict' toggle true # コマンドラインを編集中は予測入力をしない
zstyle ':predict' verbose true # 予測入力のon/offが切り替わった時に表示する

zstyle ':completion:*' list-colors '' # zsh補完候補一覧をカラー表示
#zstyle ':completion:*' menu select=1 
#zstyle ':completion:*' menu select=1 eval "$(dircolors -b)" 
# dircolors -b `ls' コマンドのカラー出力の設定 <- を evalすることで$LS_COLORSとかが環境変数に設定される
zstyle ':completion:*:default' menu select=1 # 補完メニューをカーソルで選択可能にする。 
zstyle ':completion:*:cd:*' tag-order local-directories path-directories # カレントに候補が無い場合のみcdpath 上のディレクトリが候補となる。 
#zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*' # 補完の時に大文字小文字を区別しない 
#zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}'# 補完の時に大文字小文字を区別しない 
setopt list_packed # 補完候補をつめて表示する 
setopt auto_menu # TAB で順に補完候補を切り替える 
setopt auto_list # 複数の補完候補があったときに、そのリストを自動的に表示 
setopt complete_in_word # 補完開始時にカーソルは単語の終端になくても良い。 
setopt LIST_TYPES # 補完候補を種別表示(ls -F)
setopt auto_param_keys # カッコの対応などを自動的に補完 
#setopt auto_param_slash # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える 
setopt MAGIC_EQUAL_SUBST # = 以降も補完 (--prefix=/usr等)
setopt correct # 入力したコマンド名が間違っている場合には修正を提案
setopt correct_all # リターンを押す度にスペルチェックをする。 
setopt NUMERIC_GLOB_SORT # 数値ファイル名マッチ時は数値順にソート
setopt PRINT_EIGHT_BIT # 補完リストその他でもASCII(7ビット)以上の文字(8ビット)文字を表示(マルチバイト文字補完)

## コマンドライン cd関連 
cdpath=($HOME) # カレントディレクトリにサブディレクトリが見つからない場合にcdが検索する場所 
setopt auto_cd # そのディレクトリへのcdコマンドが実行される。 
setopt auto_pushd # [cd -TAB] でディレクトリを記録したディレクトリを表示する。 
setopt pushd_ignore_dups # 同じディレクトリを pushd しない 
setopt nolistbeep # ビープ音カット 
setopt noautoremoveslash # パスの最後の/を削除しない（たまに/でコマンドの意味が変化するので有効推奨）
setopt check_jobs # ジョブ制御を行っているシェルが終了する前に、バックグラウンドやサスペンド状態のジョブの状態を報告する。 
setopt chase_links # シンボリックリンクを正しいパスに変更する。 
setopt extended_history # コマンド開始時のタイムスタンプと、実行時間(秒)をヒストリファイルに書き込む。 

## その他 
bindkey -e # キーバインドをemacs風に
setopt NO_FLOW_CONTROL # Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NOTIFY # バックグラウンドジョブ状態の即時報告
setopt interactive_comments # コマンドラインでも # 以降をコメントと見なす
setopt brace_ccl # {a-c} を a b c に展開する機能を使えるようにする 
setopt extended_glob # globを拡張する。

a2Z() { tr "[a-z]" "[A-Z]" } # 大文字へ変換
A2z() { tr "[A-Z]" "[a-z]" } # 小文字へ変換
#1つ前のコマンドの最後の単語をctrl-]で挿入する
autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match \
  '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word

#一つ前の単語を ' で囲む ctrl+s
#一つ前の単語を " で囲む ctrl+d
# quote previous word in single or double quote
autoload -U modify-current-argument
_quote-previous-word-in-single() {
    modify-current-argument '${(qq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-single
bindkey '^[s' _quote-previous-word-in-single

_quote-previous-word-in-double() {
    modify-current-argument '${(qqq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-double
bindkey '^[d' _quote-previous-word-in-double

#################################################
# プロンプト表示フォーマット
# http://zsh.sourceforge.net/Doc/Release/zsh_12.html#SEC40
#################################################
# %% %を表示
# %) )を表示
# %l 端末名省略形
# %M ホスト名(FQDN)
# %m ホスト名(サブドメイン)
# %n ユーザー名
# %y 端末名
# %# rootなら#、他は%を表示
# %? 直前に実行したコマンドの結果コード
# %d ワーキングディレクトリ %/ でも可
# %~ ホームディレクトリからのパス
# %h ヒストリ番号 %! でも可
# %a The observed action, i.e. "logged on" or "logged off".
# %S (%s) 反転モードの開始/終了 %S abc %s とするとabcが反転
# %U (%u) 下線モードの開始/終了 %U abc %u とするとabcに下線
# %B (%b) 強調モードの開始/終了 %B abc %b とするとabcを強調
# %t 時刻表示(12時間単位、午前/午後つき) %@ でも可
# %T 時刻表示(24時間表示)
# %* 時刻表示(24時間表示秒付き)
# %w 日表示(dd) 日本語だと 曜日 日
# %W 年月日表示(mm/dd/yy)
# %D 年月日表示(yy-mm-dd)
case ${UID} in
0) #rootユーザ用
  PROMPT="%{${fg[red]}%}%m%#%{${reset_color}%} "
# SSH接続時
#  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
#    PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
  ;;
*) #一般ユーザ用
  PROMPT="%{${fg[cyan]}%}%m%#%{${reset_color}%} "
#  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
#    PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
  ;;
esac
if [ "$EMACS" ];then
    # Emacs の ansi-term では右プロンプトを表示しない
    RPROMPT=""
else
    RPROMPT="[%~]"
fi

#################################################
#未整理
#################################################

# xtermのタイトル設定
# 基礎知識 Xterm Control Sequences参照(http://www.xfree86.org/current/ctlseqs.html),screen(1)参照
# エスケープシークエンス <esc>=033=\e <bell>=007=\a <backslash>=134=\\ ascii(7)参照
# xterm,screenの制御文字 <string terminator>=<esc><backslash>,<operating system command>=<esc>
#
# printf "\e0;文字列\a" でxtermのウィンドウタイトルを文字列に設定する。
#
# screenに文字列を評価させずにxtermに文字列を渡す(\eP文字列\e\\で渡せる)ことによって
# xtermのタイトルを変更できる。上記とあわせて。
# 例:printf "\eP\e0;文字列\a\e\\"
case "${TERM}" in
kterm*|xterm*)
precmd() {
    echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
  }
;; 
screen*) # screen
# preexec() {
#     printf "\eP\e]0;!${1%% *}\a\e\\" # 実行中
#     }
precmd() {
    printf "\eP\e]0;${USER}@${HOST%%.*}:${PWD}\a\e\\"
}
    ;;
esac

#~/.zshrcから~/.zshrc.mineファイルの内容を読み込んで実行
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine

#end
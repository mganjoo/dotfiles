# vim: foldmethod=marker:fen ft=zsh
# Litra Glow control
# https://ultracrepidarian.phfactor.net/2022/03/09/controlling-the-logitech-litra-on-macos/

function _hid() {
  if (( $+commands[hidapitester] )); then
    hidapitester --vidpid 046D/C900 --open --length 20 --send-output $1
  fi
}

# 2/17/22 Litra Glow aliases from https://github.com/kharyam/litra-driver/issues/13
function light() {
  _hid 0x11,0xff,0x04,0x1c,0x01
}
function dark() {
  _hid 0x11,0xff,0x04,0x1c
}
# ~10%
function glow(){
  _hid 0x11,0xff,0x04,0x4c,0x00,20
}
# ~20%
function dim(){
  _hid 0x11,0xff,0x04,0x4c,0x00,50
}
# tweaking by hand - less than 50%
function normal() {
  _hid 0x11,0xff,0x04,0x4c,0x00,70
}
# ~50%
function medium() {
  _hid 0x11,0xff,0x04,0x4c,0x00,100
}
# 90%
function bright(){
  _hid 0x11,0xff,0x04,0x4c,0x00,204
}
# 2700K
function warmest() {
  _hid 0x11,0xff,0x04,0x9c,10,140
}
# 3200K
function warm() {
  _hid 0x11,0xff,0x04,0x9c,12,128
}
# 6500K
function coldest() {
  _hid 0x11,0xff,0x04,0x9c,25,100
}
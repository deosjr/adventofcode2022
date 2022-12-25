package main

import (
    "testing"
)

func TestDec2Snafu(t *testing.T) {
    for _, tt := range []struct{
        decimal int
        want    string
    }{
        {        1,             "1"},
        {        2,             "2"},
        {        3,            "1="},
        {        4,            "1-"},
        {        5,            "10"},
        {        6,            "11"},
        {        7,            "12"},
        {        8,            "2="},
        {        9,            "2-"},
        {       10,            "20"},
        {       15,           "1=0"},
        {       20,           "1-0"},
        {     2022,        "1=11-2"},
        {    12345,       "1-0---0"},
        {314159265, "1121-1110-1=0"},
    }{
        got := dec2snafu(tt.decimal)
        if got != tt.want {
            t.Errorf("got %s want %s", got, tt.want)
        }
    }
}

func TestSnafu2Dec(t *testing.T) {
    for _, tt := range []struct{
        want  int
        snafu string
    }{
        {        1,             "1"},
        {        2,             "2"},
        {        3,            "1="},
        {        4,            "1-"},
        {        5,            "10"},
        {        6,            "11"},
        {        7,            "12"},
        {        8,            "2="},
        {        9,            "2-"},
        {       10,            "20"},
        {       15,           "1=0"},
        {       20,           "1-0"},
        {     2022,        "1=11-2"},
        {    12345,       "1-0---0"},
        {314159265, "1121-1110-1=0"},
    }{
        got := snafu2dec(tt.snafu)
        if got != tt.want {
            t.Errorf("got %d want %d", got, tt.want)
        }
    }
}

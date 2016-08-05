$ErrorActionPreference = 'Stop'

Push-Location "$PSScriptRoot/src"
try {
    stack build
    Get-ChildItem '../problems' '*.txt' | % {
        $target = [IO.Path]::GetFileNameWithoutExtension($_.Name)
        stack exec visualize -- $_.FullName -o ../svg/$target.svg -w 500
    }
} finally {
    Pop-Location
}

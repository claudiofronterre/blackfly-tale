from pathlib import Path
import base64
assets = Path(__file__).parent / 'assets'
styles = ''
for name, weight in [('Manrope-Regular.ttf', 400), ('Manrope-Bold.ttf', 700)]:
    font = base64.b64encode((assets / name).read_bytes()).decode()
    styles += f"@font-face{{font-family:Manrope;font-weight:{weight};src:url(data:font/ttf;base64,{font}) format('truetype');}}"
for path in assets.glob('*.svg'):
    if path.stem == 'birmingham-logo':
        continue
    source = path.read_text()
    if 'data:font/ttf;base64,' in source:
        continue
    insertion = source.find('>', source.find('<svg')) + 1
    path.write_text(source[:insertion] + '<style>' + styles + '</style>' + source[insertion:])

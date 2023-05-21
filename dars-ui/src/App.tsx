import React from 'react';
import { Button, Navbar, Theme } from 'react-daisyui'
import { useTranslation } from "react-i18next";
import i18n from './i18n';
import './App.css';

function App() {
  const { t } = useTranslation();

  const onClickLanguage = (lang: string) => () => {
    i18n.changeLanguage(lang);
  }

  return (
    <div className="App">
      <Theme dataTheme="light">
        <Navbar>
        <Button color="primary" size="xs" onClick={onClickLanguage("en")}>
          English
        </Button>
        <Button color="primary" size="xs" onClick={onClickLanguage("ru")}>
          Русский
        </Button>
        </Navbar>
        {t("click")}
      </Theme>
    </div>
  );
}

export default App;
